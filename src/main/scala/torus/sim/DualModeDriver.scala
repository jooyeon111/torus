package torus.sim

import spinal.core._
import spinal.core.sim._

import torus.arithmetic._
import torus.array._
import torus.config._
import torus.core._

/**
 * Simulation driver for [[ToroidalSystolicArray]] with `dualMode` in DUAL mode — ReuseB and
 * ReuseA, with or without the two-big-SRAM accumulator.
 *
 * ══ What dual mode computes ══
 *
 * Dual mode is the low-M GEMV schedule, NOT the normal GEMM schedule:
 *
 *   C[1×N] = A[1×K] · B[K×N]        (M == 1 is a hard requirement)
 *
 *   - A (the input matrix, one row) is loaded through the STATIONARY path with
 *     `stationarySelect = broadcast`, so [[StationaryDemux]] copies the single
 *     `multPerPe`-wide group across every group of the broadcast axis. Every group
 *     therefore holds the SAME A vector — which is what the N outputs need, since
 *     they all contract against the same A. For ReuseB that means A occupies the
 *     port B normally would (the operands swap roles); for ReuseA the stationary
 *     operand is A already, and dual mode only makes it a broadcast single row.
 *   - B streams on the per-VectorPE second stream — `inputA2` for ReuseB, `inputB2`
 *     for ReuseA — with `select = 1` so each MAC takes its dedicated lane instead of
 *     the stock systolic edge.
 *
 * ══ The ReuseA / ReuseB mirror ══
 *
 * The two dataflows are reflections of each other, and everything below is written in
 * terms of two abstract axes: the CONTRACTION axis the stationary operand is chained
 * along, and the PORT axis the outputs come out on.
 *
 *                     ReuseB                          ReuseA
 *   contraction       rows   (g = r*lvR + a)          columns (g = c*lvC + b)
 *   ports             cols   (q = c*lvC + b)          rows    (q = r*lvR + a)
 *   chain length L    totalVectorPeRow                totalVectorPeCol
 *   second stream     inputA2 / select                inputB2 / select
 *   stock edge        inputA (held at 0)              inputB (held at 0)
 *   weight enable     inputUpdateEnableB              inputUpdateEnableA
 *   dual reduction    adder tree over rows            adder tree over columns
 *
 * In both cases `kTile = L * multPerPe` (the array's contraction capacity, i.e.
 * `contractionPassDepth`) and one port emits a complete result EVERY cycle, so N
 * streams over time as n = t * numPorts + q. Unlike the stock dataflows (where one
 * row/column of the streamed operand trickles through and the array idles), every PE
 * does a useful MAC on every cycle.
 *
 * ══ Timing ══
 *
 * The per-PE lanes carry no skew anywhere: the second stream is consumed fully
 * aligned at every MAC and reduced by the array-level [[DualModeAdderTree]], so the
 * harness drives ALL lanes of a stream step on the same cycle. The output latency is
 *
 *   ReuseB: 1 + outerPeRow + 1 + log2(multPerPe) + log2(localVectorPeRow)
 *   ReuseA: 1 + outerPeCol + 1 + log2(multPerPe) + log2(localVectorPeCol)
 *
 * (the second stream's registered entry stage at the DualMode boundary, the in-PE
 * product register, the two pipelined adder trees, and the phase-1 compensation delay
 * padding the tree path to exactly the stock chain's depth — which is why the formula
 * still reads as one register per outer hop). That entry stage mirrors the stock
 * operand's own input [[RegisterBuffer]], so both paths are equally deep and
 * `accControl` keeps stock timing. The checks do not trust the formula: they locate
 * the alignment empirically and report whether it matched.
 *
 * ══ The two run modes ══
 *
 *  - `needAccumulator = false` ([[runGemvNoAcc]]): each K-tile is a self-contained burst
 *    and the raw per-cycle partial sums are checked exactly, then summed across
 *    K-tiles in software and compared against the full C.
 *
 *  - `needAccumulator = true` ([[runGemvAcc]]): the K-passes accumulate in the AccSram
 *    instead, and only the drained totals are checked. Because M is 1, the
 *    accumulator's FREE output dimension is N, so `freeDimensionSize` is the number
 *    of STREAM STEPS per K-pass and one panel spans `freeDimensionSize * numPorts`
 *    output elements.
 *
 * ══ Stationary reuse ══
 *
 * Both paths hold a loaded A slice for as many B columns as the schedule can reach,
 * which is the whole point of loading it: `loadStationary` costs `chainLen + settle`
 * cycles and only a long stream amortises it.
 *
 *  - no-acc: K-tile outer, the FULL N streamed per load — one load per K-tile.
 *  - acc: the resident panel bounds it. A panel is `freeDimensionSize * numPorts`
 *    outputs wide, K-passes run inside it, and A is re-loaded once per (panel, pass)
 *    because the next pass needs a different A slice in the chain. So a load is
 *    amortised over one panel, and provisioning `freeDimensionSize >= N / numPorts`
 *    (one resident panel for the whole N) is what makes the acc schedule match the
 *    no-acc one: one load per K-pass, the whole N streamed against it. A panel
 *    narrower than N still works and just reloads A per panel.
 */
object DualModeDriver {

  /** Dispatch one dual-mode config through the low-M GEMV schedule. Anything the
    * dual-mode path does not cover yet is a [[Verdict.Skip]] rather than a failure. The
    * mode-switch schedules have their own entry point, [[runSwitch]]. */
  def run(cfg: ToroidalDualModeConfig, cfgName: String, gemm: GemmDim): Verdict = {

    if (cfg.numericType != NumericType.Signed) skip(cfgName, gemm, "dual-mode driver supports Signed only")
    else if (!cfg.isReuseAB)                   skip(cfgName, gemm, s"dual mode supports ReuseA / ReuseB only, got ${cfg.dataflow}")
    else if (gemm.M != 1)                      skip(cfgName, gemm, s"dual mode requires M == 1, got M=${gemm.M}")
    else if (cfg.needAccumulator)              runGemvAcc(cfg, cfgName, gemm)
    else                                       runGemvNoAcc(cfg, cfgName, gemm)
  }

  /** One `[skip]` line, and the verdict that carries its reason. */
  private def skip(cfgName: String, gemm: GemmDim, why: String): Verdict = {
    println(s"  [skip] $cfgName × ${gemm.name} — $why")
    Verdict.Skip(why)
  }

  private def ceilDiv(x: Int, y: Int): Int = (x + y - 1) / y

  // Geometry and stimulus live in [[SimHarness]]: the dual-mode array runs TWO
  // schedules, and the mode-switch runner drives both on one DUT, so neither the
  // second-stream `lanes` mapping nor the poker can belong to one schedule's driver.

  /** Random A[1×K] / B[K×N] and the reference C[1×N]. */
  private final class Golden(gemm: GemmDim) {
    val matA: Matrix = Matrix.random(1, gemm.K)
    val matB: Matrix = Matrix.random(gemm.K, gemm.N)
    val refC: Array[BigInt] = Array.tabulate(gemm.N) { n =>
      var acc = 0
      for (k <- 0 until gemm.K) acc += matA(0, k) * matB(k, n)
      BigInt(acc)
    }
    def a(k: Int): Int          = if (k < gemm.K) matA(0, k) else 0
    def b(k: Int, n: Int): Int  = if (k < gemm.K && n < gemm.N) matB(k, n) else 0
  }

  // ════════════════════════════════════════════════════════════
  //  No accumulator: each K-tile checked exactly, summed in software
  // ════════════════════════════════════════════════════════════

  private def runGemvNoAcc(cfg: ToroidalDualModeConfig, cfgName: String, gemm: GemmDim): Verdict = {

    // Dual mode is a flag on the stock Toroidal array; everything class-dispatched
    // (topology, port types, definition name) runs off the plain config.
    val fabric = cfg.toToroidalConfig

    implicit val arith: Arithmetic[SInt, SInt]       = Arithmetic.sIntArithmetic
    implicit val pt:    PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(fabric)

    val g      = new SimHarness.ToroidalGeometry(cfg)
    val golden = new Golden(gemm)

    val numKTiles    = ceilDiv(gemm.K, g.kTile)
    val streamCycles = ceilDiv(gemm.N, g.numPorts)
    val drainTail    = g.latency + 8

    /** Golden partial C for one K-tile, indexed [stream step][port]. */
    def expectedOf(kt: Int): Array[Array[BigInt]] =
      Array.tabulate(streamCycles, g.numPorts) { (t, q) =>
        val n = t * g.numPorts + q
        if (n >= gemm.N) BigInt(0)
        else {
          var acc = 0
          for (kk <- 0 until g.kTile) acc += golden.a(kt * g.kTile + kk) * golden.b(kt * g.kTile + kk, n)
          BigInt(acc)
        }
      }

    /** Capture offset at which the whole expected window matches. */
    def alignmentOf(captured: Array[Array[BigInt]], expected: Array[Array[BigInt]]): Option[Int] =
      (0 to captured.length - streamCycles).find { d =>
        (0 until streamCycles).forall(t => (0 until g.numPorts).forall(q => captured(d + t)(q) == expected(t)(q)))
      }

    var allPass = true
    // Captured (not recomputed) partials, summed across K-tiles for the end-to-end check.
    val accumulated = Array.fill(gemm.N)(BigInt(0))

    SimConfig.compile(new ToroidalSystolicArray[SInt, SInt](fabric, dualMode = true)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val poke = new SimHarness.ToroidalPoker(dut, g)
      // Held for the whole run: dual stream at every MAC, stationary group broadcast
      // across the whole broadcast axis.
      poke.setMode(Schedule.Gemv)

      for (kt <- 0 until numKTiles) {
        // No accumulator: each K-tile is a self-contained burst, so no state carries
        // over. The reset is the CALLER's — the phase body owns neither it nor the
        // elaboration, which is what makes the same body serve the mode-switch run.
        poke.reset()
        val captured = SimHarness.phaseGemv(
          poke,
          stationaryAt = (slot, p) => golden.a(kt * g.kTile + slot * g.mult + p),
          streamAt     = (k, n)    => golden.b(kt * g.kTile + k, n),
          steps        = streamCycles,
          drainTail    = drainTail,
        )
        val captureCycles = captured.length

        //  Exact per-cycle compare against the golden partial C for this K-tile.
        val expected = expectedOf(kt)
        val label    = s"$cfgName dual ${cfg.dataflow} k-tile $kt/$numKTiles"
        alignmentOf(captured, expected) match {
          case None =>
            allPass = false
            println(s"      [fail] $label: no capture offset matches the golden partial C")
            reportMismatch(captured, expected, streamCycles, g.numPorts, g.latency)

          case Some(d) =>
            // Nothing may appear outside the aligned window — a wrong alignment or a
            // leaking pipeline would show up here.
            val stray = (0 until captureCycles)
              .filter(t => t < d || t >= d + streamCycles)
              .filter(t => captured(t).exists(_ != 0))
            if (stray.nonEmpty) {
              allPass = false
              println(s"      [fail] $label: non-zero output outside the aligned window at cycles ${stray.mkString(", ")}")
            } else {
              val note = if (d == g.latency) "" else s" (predicted ${g.latency})"
              println(s"      [pass] $label: ${streamCycles * g.numPorts} outputs exact, latency $d$note")
            }
            for (t <- 0 until streamCycles; q <- 0 until g.numPorts) {
              val n = t * g.numPorts + q
              if (n < gemm.N) accumulated(n) += captured(d + t)(q)
            }
        }
      }
    }

    //  End-to-end: the K-tile partials the DUT produced must sum to the full C.
    if (allPass) allPass = compareTotals(accumulated, golden.refC, gemm.N, "summed K-tiles")

    println(f"  ${if (allPass) "PASS" else "FAIL"}  $cfgName (Toroidal DualMode ${cfg.dataflow}, no-acc)  ${gemm.name} " +
      s"(M=${gemm.M}, N=${gemm.N}, K=${gemm.K}; kTile=${g.kTile}, ports=${g.numPorts}, " +
      s"kTiles=$numKTiles, streamCycles=$streamCycles)")
    DataflowDriver.verdictOf(allPass)
  }

  // ════════════════════════════════════════════════════════════
  //  Two-big-SRAM accumulator: K-passes summed in the AccSram
  // ════════════════════════════════════════════════════════════

  /**
   * Run the low-M GEMV through the output-indexed two-big-SRAM accumulator.
   *
   * With M == 1 the accumulator's free output dimension is N, so one AccSram slot
   * holds one STREAM STEP's worth of outputs (`numPorts` elements) and a panel spans
   * `depth * numPorts` of C. Each K-pass re-loads the stationary A slice and streams
   * the panel's B slice for exactly `depth` psumValid cycles — exactly one address
   * wrap, which the free-running counter requires — accumulating in place; the panel
   * then drains as one ping-pong burst.
   *
   * The burst length is fixed at `depth`, but the panel does NOT have to be full: a
   * step whose output index has run past N streams B = 0 (`Golden.b` pads), so that
   * slot accumulates zero and drains zero. `freeDimensionSize` is a provisioning cap,
   * not the GEMM's extent (see [[SystolicArrayConfig.freeDimensionSize]]) — which is
   * what lets a config provision one panel wide enough for the whole N and pay the
   * stationary load only once per K-pass, instead of once per (panel, pass).
   *
   * `accControl` needs no dual-mode-specific offset: it is driven on the same cycle as
   * its stream step, exactly as in the stock dataflows. That holds because the second
   * stream now crosses its own registered entry stage at the DualMode boundary
   * (`dualStreamBuffer`), making the dual path as deep as the stock operand path the
   * control delay chain is tuned for. Before that stage existed the dual data arrived a
   * cycle early and the control had to lead it by one.
   */
  private def runGemvAcc(cfg: ToroidalDualModeConfig, cfgName: String, gemm: GemmDim): Verdict = {

    // Dual mode is a flag on the stock Toroidal array; everything class-dispatched
    // (topology, port types, definition name) runs off the plain config.
    val fabric = cfg.toToroidalConfig

    implicit val arith: Arithmetic[SInt, SInt]       = Arithmetic.sIntArithmetic
    implicit val pt:    PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(fabric)

    val g     = new SimHarness.ToroidalGeometry(cfg)
    val depth = cfg.freeDimensionSize      // AccSram depth == stream steps per K-pass
    val span  = depth * g.numPorts         // C elements one resident panel covers

    // The address counter free-runs and wraps at `depth`, so a K-pass must drive
    // EXACTLY `depth` psumValid cycles — a shorter burst would permanently shift the
    // slot mapping. The panel it fills may still be partial: the steps past N stream
    // B = 0 and their slots accumulate and drain zero. So N need not tile the panel,
    // only the burst length is fixed.
    if (gemm.K > cfg.maxContractionSize)
      return skip(cfgName, gemm, s"K=${gemm.K} exceeds maxContractionSize=${cfg.maxContractionSize} " +
        "(the accumulator data width is provisioned from it)")

    val golden     = new Golden(gemm)
    val numPanels  = ceilDiv(gemm.N, span)
    val numKPasses = ceilDiv(gemm.K, g.kTile)
    // The panel drains in `depth` cycles (one address wrap); add the pipe tail and
    // slack for the 2-cycle sync-SRAM read pipeline.
    val drainWindow = depth + g.latency + 12

    var allPass = true

    SimConfig.compile(new ToroidalSystolicArray[SInt, SInt](fabric, dualMode = true)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val poke = new SimHarness.ToroidalPoker(dut, g)
      poke.setMode(Schedule.Gemv)

      // One reset only. Consecutive panels alternate banks via the writeSel/drainSel
      // ping-pong, and a reset would force writeSel back to bank 0 and defeat it.
      poke.reset()

      for (panel <- 0 until numPanels) {
        val base = panel * span

        //  Every K-pass of this panel, then a blocking drain. The panel body is shared
        //  with the accumulator mode-switch phases, which is what keeps them wiring
        //  rather than a fourth implementation of the two schedules.
        val captured = SimHarness.phaseGemvAccPanel(
          poke,
          stationaryAt = (pass, slot, p) => golden.a(pass * g.kTile + slot * g.mult + p),
          streamAt     = (pass, k, n)    => golden.b(pass * g.kTile + k, n),
          base         = base,
          depth        = depth,
          numKPasses   = numKPasses,
          drainWindow  = drainWindow,
        )

        val last  = math.min(base + span, gemm.N) - 1
        val label = s"$cfgName dual ${cfg.dataflow} acc panel $panel/$numPanels (n=$base..$last)"
        // Slot `s` holds C[base + s*numPorts + q] on port `q` — or zero once that index
        // has run past N, since a partly-filled panel streamed zeros into those slots.
        def want(slot: Int, q: Int): BigInt = {
          val idx = base + slot * g.numPorts + q
          if (idx < gemm.N) golden.refC(idx) else BigInt(0)
        }
        allPass &= SimHarness.compareExactDrain(label, want, depth, g.numPorts, captured).isDefined
      }
    }

    // `loads` is the stationary reuse figure: one load per (panel, pass), each amortised
    // over `min(span, N)` outputs. It bottoms out at numKPasses once a panel covers N.
    println(f"  ${if (allPass) "PASS" else "FAIL"}  $cfgName (Toroidal DualMode ${cfg.dataflow}, big SRAM)  ${gemm.name} " +
      s"(M=${gemm.M}, N=${gemm.N}, K=${gemm.K}; kTile=${g.kTile}, ports=${g.numPorts}, " +
      s"depth=$depth, span=$span, panels=$numPanels, kPasses=$numKPasses, " +
      s"loads=${numPanels * numKPasses})")
    DataflowDriver.verdictOf(allPass)
  }

  private def compareTotals(got: Array[BigInt], want: Array[BigInt], n: Int, what: String): Boolean = {
    val wrong = (0 until n).filter(i => got(i) != want(i))
    if (wrong.isEmpty) {
      println(s"      [pass] $what == reference C ($n elements)")
      true
    } else {
      println(s"      [fail] $what != reference C at n = ${wrong.take(8).mkString(", ")}" +
        (if (wrong.length > 8) s", ... (${wrong.length} total)" else ""))
      wrong.take(4).foreach(i => println(s"        n=$i: got ${got(i)}, want ${want(i)}"))
      false
    }
  }

  /** Print the first few offending cycles at the predicted latency, to make a failure
    * readable without re-running with a waveform. */
  private def reportMismatch(
    captured: Array[Array[BigInt]],
    expected: Array[Array[BigInt]],
    streamCycles: Int,
    numPorts: Int,
    latency: Int,
  ): Unit = {
    var shown = 0
    for (t <- 0 until streamCycles if shown < 4) {
      val idx = t + latency
      if (idx < captured.length && (0 until numPorts).exists(q => captured(idx)(q) != expected(t)(q))) {
        println(s"        step $t (capture cycle $idx):")
        println(s"          got  ${captured(idx).mkString(", ")}")
        println(s"          want ${expected(t).mkString(", ")}")
        shown += 1
      }
    }
  }


  // ════════════════════════════════════════════════════════════
  //  Schedule.SwitchGemmFirst / SwitchGemvFirst: both modes on ONE array, ONE reset
  // ════════════════════════════════════════════════════════════

  /**
   * The accumulator halves of the mode switch — the same two orders, with the
   * two-big-SRAM accumulator attached (cases 9 and 10 of the matrix).
   *
   * ══ Why this is legal at all ══
   *
   * It used to be refused, citing the AccSram's free-running address counter: a K-pass is
   * tied to an exact burst length, and the two schedules measure that burst in different
   * UNITS — stock GEMM in `slotStride`-sized slots, GEMV in single stream steps. That is a
   * correct statement about INTERLEAVING two schedules inside one panel. It is not a
   * statement about running one completed panel of each, back to back, and three facts in
   * the RTL say the sequential form is sound:
   *
   *  1. the counter does not advance on idle cycles — [[AccSram]] steps `addr` only
   *     `when(shiftEnable)`, i.e. its bank-local `accControl.psumValid`, which the double
   *     buffer raises only for `psumValid` or `drainActive` — so the idle window
   *     between two phases moves nothing and a mode change costs no address drift;
   *  2. both schedules already drive whole multiples of `depth`. Each maintains that for
   *     its own reasons (GEMM pads unused slots with zeros so the burst is
   *     `numSlots * slotStride == depth`; GEMV streams `depth` steps with the tail
   *     zero-fed). The differing UNIT is how each computes its burst; the LENGTH is the
   *     same quantity, and the length is what the counter cares about;
   *  3. [[AccSramDoubleBuffer]] states the closure property itself: a panel accumulates
   *     over a whole multiple of `depth` cycles, so the drain reads from the same
   *     rotational offset the accumulate started at.
   *
   * ══ Why phase 3 is checkable ══
   *
   * `burstEnded` flips `writeSel` and hands the filled bank to `drainSel`, so with three
   * phases each ending in exactly one completed panel the banks go 0, 1, 0: phase 3 lands
   * back on phase 1's bank, at the rotational offset it started from. An ODD phase count
   * is what buys this — a four-phase sequence would not have it — which is why phase 3
   * must reproduce phase 1's ROTATION as well as its words. The rotation is the observable
   * that carries the address-counter state across the mode change, and asserting it is how
   * this case earns its place rather than merely re-running two schedules.
   *
   * ══ What it does NOT prove ══
   *
   * Nothing about SRAM residue. The first K-pass of every phase holds `firstLoop` high for
   * the whole burst, so `sumEnable` is low and every slot is OVERWRITTEN rather than
   * accumulated — and the drain is a read-and-zero pass on top of that. The accumulator
   * contents are cleared twice over by construction. The claim here is about the fabric
   * and the control path — the mode selects, the `accControl` delay chain, the AccTap path
   * and the bank ping-pong surviving a live mode change — exactly as in the no-acc orders.
   * A reader who assumes otherwise will over-trust a pass.
   *
   * ══ One panel per phase ══
   *
   * Load-bearing, not tidiness. A phase that ended mid-panel would break all three facts
   * above at once, and a multi-panel phase would test the panel sequence AND the mode
   * switch in one verdict. Both preconditions below are skips.
   */
  private def runSwitchAcc(
    cfg:     ToroidalDualModeConfig,
    cfgName: String,
    spec:    GemmSpec,
    order:   Schedule.Value,
  ): Verdict = {
    val gemm = spec.primary
    if (!cfg.isReuseAB) return skip(cfgName, gemm, s"dual mode supports ReuseA / ReuseB only, got ${cfg.dataflow}")

    val gemvDim = spec.secondary match {
      case Some(g) => g
      case None    => return skip(cfgName, gemm,
        "a mode-switch row needs a GEMV shape file as well as a GEMM one")
    }

    val fabric = cfg.toToroidalConfig

    implicit val arith: Arithmetic[SInt, SInt]       = Arithmetic.sIntArithmetic
    implicit val pt:    PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(fabric)

    val g      = new SimHarness.ToroidalGeometry(cfg)
    val golden = new GoldenMatrix(gemm, fabric)
    val tiles  = golden.shapedTiles

    val depth      = cfg.freeDimensionSize
    val slotStride = if (g.reuseB) golden.tileShape.tileM else golden.tileShape.tileN
    // The stock burst must fill exactly one address wrap. On a dual config this is a skip,
    // not a require.
    if (depth % slotStride != 0)
      return skip(cfgName, gemm, s"the per-K-pass psumValid burst must equal the AccSram depth " +
        s"($depth), but the slot stride ($slotStride) does not divide it")
    val numSlots = depth / slotStride

    def fixedOf(t: GoldenMatrix.ShapedTile): Int = if (g.reuseB) t.index.n else t.index.m
    def sweptOf(t: GoldenMatrix.ShapedTile): Int = if (g.reuseB) t.index.m else t.index.n
    val fixedIdx = tiles.map(fixedOf).distinct
    val sweptIdx = tiles.map(sweptOf).distinct.sorted

    if (fixedIdx.size != 1 || sweptIdx.size > numSlots)
      return skip(cfgName, gemm, s"the GEMM shape ${gemm.name} cuts ${fixedIdx.size} panel(s) of " +
        s"${sweptIdx.size} slot(s) against $numSlots, and a switch phase must be exactly ONE panel — " +
        "a multi-panel phase would test the panel sequence and the mode switch in one verdict")

    // The stationary chain must be fully written by each phase: a partly loaded chain
    // would leave the previous mode's values in the unwritten slots.
    val tile0 = tiles.head
    val normalLoadSteps = if (g.reuseB) tile0.b.rows else tile0.a.cols
    if (normalLoadSteps != g.chainLen)
      return skip(cfgName, gemm, s"the shaped stationary tile loads $normalLoadSteps of the ${g.chainLen} " +
        s"chain slots — pick a GEMM whose K fills one contraction pass (${cfg.contractionPassDepth})")

    // The accumulator data width is provisioned from maxContractionSize.
    if (gemm.K > cfg.maxContractionSize)
      return skip(cfgName, gemm, s"K=${gemm.K} exceeds maxContractionSize=${cfg.maxContractionSize}")
    if (gemvDim.M != 1)
      return skip(cfgName, gemm, s"the GEMV shape of a switch must have M == 1, got M=${gemvDim.M}")
    if (gemvDim.K > cfg.maxContractionSize)
      return skip(cfgName, gemm, s"the GEMV shape ${gemvDim.name} has K=${gemvDim.K}, past " +
        s"maxContractionSize=${cfg.maxContractionSize}")
    val span = depth * g.numPorts
    if (gemvDim.N > span)
      return skip(cfgName, gemm, s"the GEMV shape ${gemvDim.name} has N=${gemvDim.N}, past the $span " +
        "one resident panel spans — a switch phase must be exactly ONE panel")

    val gemv       = new Golden(gemvDim)
    val numKPasses = ceilDiv(gemvDim.K, g.kTile)

    // The panel drains in `depth` cycles (one address wrap); add the pipe tail and slack
    // for the 2-cycle sync-SRAM read pipeline. The SAME window is used as the inter-phase
    // idle, so residue that is a function of idle time cannot masquerade as a pass by
    // sitting between two differently-sized gaps.
    val window = depth + g.latency + 12

    val gemmWant = SimHarness.expectedPanelWindow(golden, fixedIdx.head, sweptIdx, slotStride, cfg.isReuseA)
    def gemvWant(slot: Int, q: Int): BigInt = {
      val idx = slot * g.numPorts + q
      if (idx < gemvDim.N) gemv.refC(idx) else BigInt(0)
    }
    def wantOf(mode: Schedule.Value): (Int, Int) => BigInt =
      if (mode == Schedule.Gemm) gemmWant else gemvWant

    val first = Schedule.firstPhaseOf(order)
    val other = if (first == Schedule.Gemm) Schedule.Gemv else Schedule.Gemm
    val what  = if (order == Schedule.SwitchGemmFirst) "Gemm→Gemv→Gemm" else "Gemv→Gemm→Gemv"

    var allPass = true

    SimConfig.compile(new ToroidalSystolicArray[SInt, SInt](fabric, dualMode = true)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val poke = new SimHarness.ToroidalPoker(dut, g)

      def enter(schedule: Schedule.Value): Unit = {
        poke.setMode(schedule)
        poke.zeroInputs()
        for (_ <- 0 until window) dut.clockDomain.waitRisingEdge()
      }

      def phase(mode: Schedule.Value): Array[Array[BigInt]] = {
        enter(mode)
        if (mode == Schedule.Gemm)
          SimHarness.phaseGemmAccPanel(poke, tiles, depth, slotStride, window)
        else
          SimHarness.phaseGemvAccPanel(
            poke,
            stationaryAt = (pass, slot, p) => gemv.a(pass * g.kTile + slot * g.mult + p),
            streamAt     = (pass, k, n)    => gemv.b(pass * g.kTile + k, n),
            base         = 0,
            depth        = depth,
            numKPasses   = numKPasses,
            drainWindow  = window,
          )
      }

      def name(mode: Schedule.Value) = if (mode == Schedule.Gemm) "normal GEMM panel" else "dual-mode GEMV panel"
      def check(mode: Schedule.Value, capture: Array[Array[BigInt]], which: String): Option[(Int, Int)] = {
        val hit = SimHarness.compareExactDrain(
          s"$cfgName switch-acc ${cfg.dataflow} $which (${name(mode)})",
          wantOf(mode), depth, g.numPorts, capture)
        allPass &= hit.isDefined
        hit
      }

      // ── one reset, at the very start — every later phase change is live ──
      poke.setMode(first)
      poke.reset()

      val r1 = check(first, phase(first), "phase 1")
      check(other, phase(other), "phase 2")
      val r3 = check(first, phase(first), "phase 3")

      // Phase 3 lands back on phase 1's bank, at the rotational offset it started from —
      // so it must reproduce phase 1's (offset, rotation) and not merely its values. The
      // rotation is what carries the address-counter state across the mode change.
      val label = s"$cfgName switch-acc ${cfg.dataflow} phase 3 vs phase 1 (bank ping-pong closes)"
      (r1, r3) match {
        case (Some(a), Some(b)) if a == b =>
          println(s"      [pass] $label: same (drain offset ${a._1}, slot rotation ${a._2})")
        case (Some(a), Some(b)) =>
          allPass = false
          println(s"      [fail] $label: phase 1 drained at (offset ${a._1}, rotation ${a._2}) " +
            s"but phase 3 at (offset ${b._1}, rotation ${b._2}) — the bank did not return to " +
            "the offset it started from")
        case _ => // a phase already failed; its own [fail] line says so
      }
    }

    println(f"  ${if (allPass) "PASS" else "FAIL"}  $cfgName (Toroidal DualMode ${cfg.dataflow}, mode switch $what, big SRAM)  " +
      s"${gemm.name} + ${gemvDim.name} (depth=$depth, slotStride=$slotStride, slots=${sweptIdx.size}/$numSlots; " +
      s"GEMV N=${gemvDim.N}, span=$span, kPasses=$numKPasses, ports=${g.numPorts})")
    DataflowDriver.verdictOf(allPass)
  }

  /** The GEMV golden C as a capture window: step `t`, port `q` carries C[t*numPorts + q],
    * or zero once that index has run past N. */
  private def expectedGemvWindow(
    golden:   Golden,
    n:        Int,
    steps:    Int,
    numPorts: Int,
  ): Array[Array[Int]] =
    Array.tabulate(steps, numPorts) { (t, q) =>
      val idx = t * numPorts + q
      if (idx < n) golden.refC(idx).toInt else 0
    }

  /**
   * Run a runtime MODE SWITCH on one elaborated array with a single reset at the very
   * start — three phases, alternating modes — so the two modes are shown to be a runtime
   * choice rather than two builds, and the third phase reproducing the first proves the
   * middle phase left nothing behind.
   *
   * `order` picks which mode the run starts (and therefore ends) in:
   *
   *   [[Schedule.SwitchGemmFirst]]  Gemm -> Gemv -> Gemm
   *   [[Schedule.SwitchGemvFirst]]  Gemv -> Gemm -> Gemv
   *
   * The mirror is not redundant, because the residue claim is DIRECTIONAL. GemmFirst
   * proves a GEMV burst leaves no state the following GEMM can see; it says nothing about
   * the converse, and the two orders use different halves of the fabric — the GEMM phase
   * writes every group of the stationary bus and streams the systolic edge, the GEMV phase
   * writes group 0 and lets [[StationaryDemux]] broadcast it. Residue from a GEMM tile
   * sitting in the second-stream registers is precisely what GemmFirst cannot see.
   *
   * ══ Two shapes, both from files ══
   *
   * `spec.primary` is the GEMM shape and `spec.secondary` the GEMV one, for both orders —
   * the field says which SHAPE it is, not which phase runs first. The GEMV phase used to
   * generate its own stimulus from a private `DualStreamSteps = 4` constant and two
   * `Matrix.random` calls; it now runs [[Golden]], the same `A[1xK] / B[KxN] / refC[1xN]`
   * the standalone GEMV cases use, over the shape the operator named. So the switch stops
   * inventing its own problem, and pointing it at a different one needs no Scala.
   *
   * ══ Preconditions, both skips ══
   *
   *  - the GEMM shape must FILL the stationary chain. Each phase reloads the whole chain
   *    for its own mode, and that full reload is what makes the switch safe: a partly
   *    loaded chain would leave the previous mode's values in the unwritten slots, which
   *    is exactly what the switch must not rely on.
   *  - the GEMV shape must fit ONE contraction pass. The standalone GEMV runner resets
   *    between K-tiles; a switch phase may not, so a multi-K-tile GEMV phase would be
   *    testing the no-reset K-tile sequence AND the mode switch in one verdict. (If that
   *    is wanted it is a separate case, not a bigger file.)
   *
   * Between phases the array only idles (inputs zeroed) for a drain window — it is never
   * re-reset — so the mode selects, which cross one register inside the array, flip live.
   * The phase bodies are the SAME ones the other two schedules run
   * ([[SimHarness.phaseGemm]] / [[SimHarness.phaseGemv]]): a phase body owns neither the
   * elaboration nor the reset, which is what makes both orders three calls each rather
   * than a second and third reimplementation.
   */
  def runSwitch(
    cfg:     ToroidalDualModeConfig,
    cfgName: String,
    spec:    GemmSpec,
    order:   Schedule.Value,
  ): Verdict = {
    if (cfg.needAccumulator) return runSwitchAcc(cfg, cfgName, spec, order)

    val gemm = spec.primary
    if (!cfg.isReuseAB) return skip(cfgName, gemm, s"dual mode supports ReuseA / ReuseB only, got ${cfg.dataflow}")

    val gemvDim = spec.secondary match {
      case Some(g) => g
      case None    => return skip(cfgName, gemm,
        "a mode-switch row needs a GEMV shape file as well as a GEMM one — add \"gemv\" to the " +
          "suite row, or pass it as the third positional argument")
    }

    val fabric = cfg.toToroidalConfig

    implicit val arith: Arithmetic[SInt, SInt]       = Arithmetic.sIntArithmetic
    implicit val pt:    PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(fabric)

    val g      = new SimHarness.ToroidalGeometry(cfg)
    val golden = new GoldenMatrix(gemm, fabric)
    val tile   = golden.shapedTiles.head

    // Normal mode loads the stationary chain from the shaped tile; the switch is only safe
    // if that covers every slot. This one stays in the runner because it needs the shaped
    // tile, which the dispatcher does not have.
    val normalLoadSteps = if (g.reuseB) tile.b.rows else tile.a.cols
    if (normalLoadSteps != g.chainLen)
      return skip(cfgName, gemm, s"the shaped stationary tile loads $normalLoadSteps of the ${g.chainLen} " +
        s"chain slots — pick a GEMM whose K fills one contraction pass (${cfg.contractionPassDepth})")

    if (gemvDim.M != 1)
      return skip(cfgName, gemm, s"the GEMV shape of a switch must have M == 1, got M=${gemvDim.M} " +
        s"in ${gemvDim.name}")
    if (gemvDim.K > g.kTile)
      return skip(cfgName, gemm, s"the GEMV shape ${gemvDim.name} has K=${gemvDim.K}, past the ${g.kTile} " +
        "one contraction pass covers. A switch phase may not reset between K-tiles, so a multi-K-tile " +
        "GEMV phase would test the K-tile sequence and the mode switch in one verdict")

    // ── GEMV phase stimulus: the shape the operator named, through the same Golden the
    //    standalone GEMV cases use ──
    val gemv         = new Golden(gemvDim)
    val gemvSteps    = ceilDiv(gemvDim.N, g.numPorts)
    val gemvExpected = expectedGemvWindow(gemv, gemvDim.N, gemvSteps, g.numPorts)
    // The STREAMED tile's extent, which is not the chain length: the chain-fill
    // precondition above is about the STATIONARY tile. They coincide only when the tile's
    // K equals its M (ReuseB) or N (ReuseA), which a dot-product config (mult > 1, so
    // kTile = chainLen * mult) does not satisfy.
    val gemmSteps    = if (g.reuseB) tile.a.rows else tile.b.cols
    val gemmExpected = SimHarness.expectedTileWindow(tile, gemmSteps, g.numPorts, cfg.isReuseA)

    val drainTail = g.drainTail
    val first     = Schedule.firstPhaseOf(order)
    val what      = if (order == Schedule.SwitchGemmFirst) "Gemm→Gemv→Gemm" else "Gemv→Gemm→Gemv"

    var allPass = true

    SimConfig.compile(new ToroidalSystolicArray[SInt, SInt](fabric, dualMode = true)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val poke = new SimHarness.ToroidalPoker(dut, g)

      /** Idle the array long enough for the previous phase to drain, and let the new
        * mode's selects (one register deep) settle before anything is loaded. */
      def enter(schedule: Schedule.Value): Unit = {
        poke.setMode(schedule)
        poke.zeroInputs()
        for (_ <- 0 until drainTail) dut.clockDomain.waitRisingEdge()
      }

      def gemmPhase(): Array[Array[BigInt]] = {
        enter(Schedule.Gemm)
        SimHarness.phaseGemm(poke, tile, drainTail)
      }
      def gemvPhase(): Array[Array[BigInt]] = {
        enter(Schedule.Gemv)
        SimHarness.phaseGemv(
          poke,
          stationaryAt = (slot, p) => gemv.a(slot * g.mult + p),
          streamAt     = (k, n)    => gemv.b(k, n),
          steps        = gemvSteps,
          drainTail    = drainTail,
        )
      }

      def phase(mode: Schedule.Value): Array[Array[BigInt]] =
        if (mode == Schedule.Gemm) gemmPhase() else gemvPhase()

      def checkAgainstGolden(mode: Schedule.Value, capture: Array[Array[BigInt]], which: String): Unit = {
        val (label, expected) =
          if (mode == Schedule.Gemm) (s"$cfgName switch ${cfg.dataflow} $which (normal GEMM tile)", gemmExpected)
          else                       (s"$cfgName switch ${cfg.dataflow} $which (dual-mode GEMV)",   gemvExpected)
        allPass &= SimHarness.compareExact(label, expected, capture).isDefined
      }

      // ── one reset, at the very start — every later phase change is live ──
      poke.setMode(first)
      poke.reset()

      //1. The starting mode, against its golden.
      val p1 = phase(first)
      checkAgainstGolden(first, p1, "phase 1")

      //2. The other mode on the same array, selects flipped live.
      val other = if (first == Schedule.Gemm) Schedule.Gemv else Schedule.Gemm
      val p2 = phase(other)
      checkAgainstGolden(other, p2, "phase 2")

      //3. The starting mode AGAIN — the residue check, and the whole claim the test exists
      //   to make. It is compared against phase 1's ACTUAL capture rather than a golden:
      //   cycle for cycle and port for port, so residue that merely permutes values across
      //   ports cannot pass. Needs no golden, and carries over to the mirror unchanged.
      val p3 = phase(first)
      val label3 = s"$cfgName switch ${cfg.dataflow} phase 3 (${if (first == Schedule.Gemm) "normal GEMM tile" else "dual-mode GEMV"} again)"
      val differing = p1.indices.filter(i => i < p3.length && !p3(i).sameElements(p1(i)))
      if (p3.length == p1.length && differing.isEmpty)
        println(s"      [pass] $label3: reproduces phase 1 cycle for cycle " +
          s"(${p1.length} cycles x ${g.numPorts} ports)")
      else {
        allPass = false
        println(s"      [fail] $label3: differs from phase 1 at cycles ${differing.take(8).mkString(", ")}")
        differing.take(3).foreach { i =>
          println(f"        cycle $i%3d: phase 1 ${p1(i).mkString(", ")}")
          println(f"        cycle $i%3d: phase 3 ${p3(i).mkString(", ")}")
        }
      }
    }

    println(f"  ${if (allPass) "PASS" else "FAIL"}  $cfgName (Toroidal DualMode ${cfg.dataflow}, mode switch $what)  " +
      s"${gemm.name} + ${gemvDim.name} (normal tile K=${golden.tileShape.tileK}, chain=${g.chainLen}, " +
      s"stream steps=$gemmSteps; GEMV kTile=${g.kTile}, N=${gemvDim.N}, K=${gemvDim.K}, " +
      s"ports=${g.numPorts}, steps=$gemvSteps)")
    DataflowDriver.verdictOf(allPass)
  }

}
