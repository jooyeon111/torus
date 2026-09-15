package torus.sim

import torus.config._
import torus.core._

/**
 * The simulation harness's DISPATCHER, and nothing else.
 *
 * [[run]] resolves one (config, [[GemmSpec]]) row to the named [[TestCase]] it covers,
 * applies every precondition that can be decided from the config and the shapes alone,
 * hands the row to a runner, and returns a [[CaseResult]]. Every runner body lives with
 * the fabric it drives:
 *
 *   - [[GroupedDriver]]  — [[GroupedSystolicArray]], ReuseA / ReuseB / ReuseC, acc or not
 *   - [[ToroidalDriver]] — [[ToroidalSystolicArray]] plain OR dual-mode-wrapped, through
 *                          the stock tiled-GEMM schedule
 *   - [[DualModeDriver]] — the schedules that use the dual hardware: the low-M GEMV and
 *                          the two mode-switch orders
 *
 * Keeping the dispatch here and the bodies there is what makes the key a TABLE: read the
 * key, derive the runner's name; read the name, know the arm.
 *
 * The block-level test of [[AccSramDoubleBuffer]] itself lives in
 * [[AccSramDoubleBufferTest]].
 */
object DataflowDriver {

  /**
   * Dispatch one row — a (config, [[GemmSpec]]) pair — to the runner that drives it, and
   * say which named [[TestCase]] it covered.
   *
   * The key is `(fabric, schedule, dataflow, needAccumulator)`, unchanged; what changed
   * is that it is now DECLARATIVE. [[TestCase.of]] names the (fabric, schedule, acc)
   * triple, so a row can be filtered by case and the summary can report the matrix
   * rather than a flat list — including the cases with NO coverage, which a flat list
   * cannot show at all.
   *
   * The body is dispatch, guards and labelling only:
   *
   *   - `fabric` reads the config's CLASS, so [[ToroidalDualModeConfig]] is reachable
   *     here at all. It extends [[SystolicArrayConfig]] directly, so it answers false
   *     to both `isGrouped` and `isToroidal` and the old three-way key skipped it.
   *   - `schedule` says WHICH schedule a dual-mode array runs. It is derived from
   *     `(fabric, M)` unless the GEMM names one — see [[GemmDim.scheduleOn]].
   *
   * An unsupported COMBINATION is a [[Verdict.Skip]] and does not fail the suite. An
   * INCONSISTENT request — a dual-mode-only schedule on a fabric with no hardware path
   * for it — keeps its `require`. Do not convert one into the other.
   */
  def run(cfg: SystolicArrayConfig, cfgName: String, spec: GemmSpec): CaseResult = {
    val gemm     = spec.primary
    val schedule = gemm.scheduleOn(cfg)

    // Gemv and the switch orders on a non-dual fabric have no hardware path — no
    // second-stream lanes, no StationaryDemux — so this is a config error, not a skip.
    require(schedule == Schedule.Gemm || cfg.isDualMode,
      s"[error] $cfgName: schedule=$schedule requires a ToroidalDualMode config, got ${cfg.fabric}")

    val kase  = TestCase.of(cfg.fabric, schedule, cfg.needAccumulator)
    val label = s"$cfgName × ${spec.name}"

    val verdict = guard(cfg, cfgName, spec, schedule) match {
      case Some(why) => skip(cfgName, spec, why)
      case None      => dispatch(cfg, cfgName, spec, schedule)
    }
    CaseResult(kase, label, verdict)
  }

  /** Print one `[skip]` line and return the verdict that carries its reason. */
  private def skip(cfgName: String, spec: GemmSpec, why: String): Verdict = {
    println(s"  [skip] $cfgName × ${spec.name} — $why")
    Verdict.Skip(why)
  }

  private[torus] def verdictOf(ok: Boolean): Verdict =
    if (ok) Verdict.Pass else Verdict.Fail("see the [fail] lines above")

  /**
   * Every precondition that can be decided from the config and the shapes alone, in one
   * place, so the dispatch below is a table rather than a table with checks in it.
   *
   * Returns the reason this row cannot be driven, or None to dispatch it.
   */
  private def guard(
    cfg:      SystolicArrayConfig,
    cfgName:  String,
    spec:     GemmSpec,
    schedule: Schedule.Value,
  ): Option[String] = {
    val gemm = spec.primary
    if (cfg.numericType != NumericType.Signed)
      Some("SRAM driver currently only supports Signed numeric")
    else noAccContractionGuard(cfg, gemm, schedule)
  }

  private def ceilDiv(x: Int, y: Int): Int = (x + y - 1) / y

  /**
   * K one pass through the fabric contracts, for every dataflow.
   *
   * [[SystolicArrayConfig.contractionPassDepth]] answers for three of the four and
   * deliberately THROWS for the fourth: Grouped ReuseC's contraction is temporal (how
   * long `accEnableC` is held) and no fabric dimension bounds a cycle count, so its bound
   * is the PE accumulator width it declares. Selecting here, once, is what keeps that
   * `require` from being reached by a guard that only wanted a number.
   */
  private def contractionPassDepthOf(cfg: SystolicArrayConfig): Int =
    if (cfg.isReuseC && cfg.isGrouped) cfg.maxContractionSize else cfg.contractionPassDepth

  /**
   * Whether the runner this row dispatches to sums the captured per-k-tile partials in
   * software and compares the TOTAL against the full C.
   *
   * Where it does, a K spanning several passes is COVERED rather than refused, and the
   * guard below never fires. Where it does not, the array emits one partial sum per pass
   * and a no-accumulator config has nothing to sum them with — so the run would report a
   * green suite for a GEMM the hardware cannot compute end to end.
   *
   * The GEMV runner had it first ([[DualModeDriver.runGemvNoAcc]]) and was the model for
   * every other no-acc runner — with ONE deliberate exception.
   *
   * TOROIDAL ReuseC is excluded even though it now sums like the rest. Its bound is the
   * wrap's cluster-loop length, an ARCHITECTURAL capacity rather than a provisioning
   * choice: a K past it is a config error, and summing the partials in software would be
   * the test harness papering over it. Software summation proves the array's PARTIALS are
   * right, not that the array can produce C — so for that one path the line stays visible.
   */
  private def sumsPartialsInSoftware(cfg: SystolicArrayConfig, schedule: Schedule.Value): Boolean =
    schedule == Schedule.Gemv ||
      (schedule == Schedule.Gemm && (cfg.isReuseAB || (cfg.isReuseC && cfg.isGrouped)))

  /**
   * The no-accumulator CONTRACTION guard.
   *
   * The bound a no-accumulator array actually runs out of hardware on is the CONTRACTING
   * dimension, not the free one: the free dimension is genuinely unbounded (a no-acc run
   * cuts one tile spanning all of it, and each stream cycle emits a complete row or
   * column of that tile's C), while one pass through the fabric contracts exactly
   * `contractionPassDepth` products. A larger K is cut into several k-tiles and the array
   * emits one PARTIAL SUM per pass — which is precisely what an accumulator is for.
   *
   * A [[Verdict.Skip]], not a `require`: an oversized K against a no-acc config is an
   * unsupported COMBINATION, not an inconsistent request.
   */
  private def noAccContractionGuard(
    cfg:      SystolicArrayConfig,
    gemm:     GemmDim,
    schedule: Schedule.Value,
  ): Option[String] =
    // A switch phase drives ONE tile and then checks that the other mode left nothing
    // behind. It makes no claim about C at all, so the contraction bound is not its
    // business — its own precondition (the shaped stationary tile must fill the chain)
    // is what keeps its K honest.
    if (cfg.needAccumulator || Schedule.isSwitch(schedule)) None
    else if (sumsPartialsInSoftware(cfg, schedule)) None
    else {
      val passDepth = contractionPassDepthOf(cfg)
      if (gemm.K <= passDepth) None
      else Some(
        s"a no-accumulator ${cfg.dataflow} array contracts $passDepth per pass, so K=${gemm.K} " +
          s"needs ${ceilDiv(gemm.K, passDepth)} passes and there is no accumulator to sum them. " +
          s"Use an accumulator config, or K <= $passDepth.")
    }

  private def dispatch(
    cfg:      SystolicArrayConfig,
    cfgName:  String,
    spec:     GemmSpec,
    schedule: Schedule.Value,
  ): Verdict = {
    val gemm = spec.primary
    (cfg.fabric, schedule, cfg.dataflow, cfg.needAccumulator) match {
      // ── the dual-mode wrapper: one fabric, several schedules on one array ──
      case (Fabric.ToroidalDual, Schedule.Gemv, _, _) =>
        DualModeDriver.run(cfg.asInstanceOf[ToroidalDualModeConfig], cfgName, gemm)
      // Both orders of the runtime mode switch, on ONE elaborated array with ONE reset,
      // with or without the accumulator. The accumulator halves used to be refused: the
      // AccSram's free-running address counter ties a K-pass to an exact burst length and
      // the two schedules measure that burst in different units — which bounds
      // INTERLEAVING them mid-panel, not SEQUENCING one completed panel of each.
      case (Fabric.ToroidalDual, s, _, _) if Schedule.isSwitch(s) =>
        DualModeDriver.runSwitch(cfg.asInstanceOf[ToroidalDualModeConfig], cfgName, spec, s)

      // ── the stock tiled GEMM on a Toroidal fabric, wrapped or not, acc or not ──
      case (Fabric.ToroidalDual | Fabric.Toroidal, Schedule.Gemm, Dataflow.ReuseA | Dataflow.ReuseB, _) =>
        ToroidalDriver.runToroidalReuseAB(cfg, cfgName, gemm)

      // ── Grouped + big-SRAM accumulator ──
      case (Fabric.Grouped, Schedule.Gemm, Dataflow.ReuseA | Dataflow.ReuseB, true) =>
        GroupedDriver.runGroupedReuseABAcc(cfg, cfgName, gemm)

      // ── Grouped ReuseA / ReuseB WITHOUT accumulator (per-tile validation) ──
      case (Fabric.Grouped, Schedule.Gemm, Dataflow.ReuseA | Dataflow.ReuseB, false) =>
        GroupedDriver.runGroupedReuseABNoAcc(cfg, cfgName, gemm)

      // ── Multi-tile ReuseC (per-tile validation against partialC, no accumulator) ──
      case (Fabric.Grouped,  Schedule.Gemm, Dataflow.ReuseC, _) => GroupedDriver.runGroupedReuseC(cfg, cfgName, gemm)
      case (Fabric.Toroidal, Schedule.Gemm, Dataflow.ReuseC, _) => ToroidalDriver.runToroidalReuseC(cfg, cfgName, gemm)

      case _ =>
        skip(cfgName, spec,
          s"(${cfg.fabric} $schedule ${cfg.dataflow}) — unsupported fabric/schedule/dataflow combination")
    }
  }
}
