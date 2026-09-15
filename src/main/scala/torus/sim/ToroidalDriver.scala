package torus.sim

import spinal.core._
import spinal.core.sim._

import torus.arithmetic._
import torus.array._
import torus.config._
import torus.core._

/**
 * Every runner that drives a [[ToroidalSystolicArray]] — plain OR dual-mode-wrapped —
 * through the stock tiled-GEMM schedule.
 *
 * The wrapper belongs here rather than in [[DualModeDriver]] because with both mode
 * selects held low the fabric IS the plain Toroidal array `toToroidalConfig` describes:
 * the demux passes its bus through, every MAC takes the stock systolic edge, and neither
 * mux adds a pipeline stage. [[DualModeDriver]] owns the schedules that actually use the
 * dual hardware.
 *
 * The bodies here are the ones [[DataflowDriver]] dispatched to before the split; the
 * dispatcher keeps only the key, the guards and the labelling.
 */
object ToroidalDriver {

  /** One `[skip]` line, and the verdict that carries its reason. */
  private def skip(cfgName: String, gemm: GemmDim, why: String): Verdict = {
    println(s"  [skip] $cfgName × ${gemm.name} — $why")
    Verdict.Skip(why)
  }

  private def verdictOf(ok: Boolean): Verdict = DataflowDriver.verdictOf(ok)

  // ── Toroidal stock GEMM: one body for the plain array and the dual-mode wrapper ──

  /**
   * Run one tiled GEMM through a Toroidal array — plain or dual-mode-wrapped, with or
   * without the two-big-SRAM accumulator.
   *
   * One body now covers five runners that used to be written out separately: Toroidal
   * ReuseA and ReuseB through the accumulator, the Toroidal half of the per-tile
   * no-accumulator path, and the dual-mode wrapper's two. They drive the SAME fabric
   * through the SAME schedule:
   *
   *  - ReuseA and ReuseB are reflections of each other, which
   *    [[SimHarness.ToroidalGeometry]] states once as a contraction axis and a port axis.
   *  - The dual-mode wrapper differs only in which top-level port carries the stationary
   *    operand, plus two mode selects this schedule holds LOW. With them low the fabric
   *    IS the plain Toroidal array of `cfg.toToroidalConfig` and neither mux adds a
   *    pipeline stage: the demux is combinational and the stationary bus still crosses
   *    the array's own input [[RegisterBuffer]].
   *
   * Only two differences survive the merge, and neither is about the schedule:
   *
   *  - the labels each run prints, which name the fabric the operator asked for;
   *  - what a slot stride that does not divide the bank depth does. The unwrapped array
   *    `require`s it (a config error); the wrapper skips it. Converting one into the
   *    other during the merge is forbidden, so it stays a branch rather than being
   *    quietly unified.
   *
   * ══ The accumulator path ══
   *
   * The output-indexed panel schedule: the stationary tile is loaded ONCE per K-pass and
   * a whole panel of the free output dimension (M for ReuseB, N for ReuseA) streams
   * through it, accumulating in the AccSram across K before the panel drains as one
   * ping-pong burst. The AccSram's address counter free-runs and wraps at `depth`, so
   * EVERY K-pass drives EXACTLY `depth` psumValid cycles — unused slots stream zeros
   * rather than shortening the burst, which would permanently shift the slot mapping.
   *
   * Drains are captured OVERLAPPED: a finished panel drains from its bank while the next
   * accumulates into the other one, so the capture rides the following panel's ticks
   * instead of a dedicated bubble. The final panel keeps a blocking drain.
   *
   * ══ The no-accumulator path ══
   *
   * No big SRAM is attached (the `outputC := collected` fall-back), so each shaped tile
   * is driven from its own reset and its capture window is compared to that tile's
   * `partialC` element for element.
   */
  def runToroidalReuseAB(cfg: SystolicArrayConfig, cfgName: String, gemm: GemmDim): Verdict = {
    require(cfg.isReuseAB,
      s"[error] the Toroidal stock-GEMM runner expects ReuseA/ReuseB, got ${cfg.dataflow}")

    val dual = cfg.isDualMode
    // Everything class-dispatched — ArrayTopology, the port types, the definition name,
    // GoldenMatrix's tiling — has a ToroidalConfig case and no ToroidalDualModeConfig
    // one, so a dual config must present its FABRIC here.
    val fabric = if (dual) cfg.asInstanceOf[ToroidalDualModeConfig].toToroidalConfig else cfg.asToroidal

    val setup = Setup(cfg, cfgName, gemm, dual, fabric,
      new SimHarness.ToroidalGeometry(cfg), new GoldenMatrix(gemm, fabric))

    // The two loops share the setup above and nothing else, not even a comparator — so
    // the shared work is written once and the branch is explicit rather than an early
    // `return` halfway down one body.
    if (cfg.needAccumulator) toroidalReuseABAcc(setup) else toroidalReuseABNoAcc(setup)
  }

  /** Everything both stock-GEMM loops need, computed once by [[runToroidalReuseAB]]. */
  private final case class Setup(
    cfg:     SystolicArrayConfig,
    cfgName: String,
    gemm:    GemmDim,
    dual:    Boolean,
    fabric:  ToroidalConfig,
    g:       SimHarness.ToroidalGeometry,
    golden:  GoldenMatrix,
  )

  /**
   * No big SRAM is attached (the `outputC := collected` fall-back), so each shaped tile
   * is driven from its own reset and its capture window is compared to that tile's
   * `partialC` element for element.
   */
  private def toroidalReuseABNoAcc(setup: Setup): Verdict = {
    import setup._

    implicit val arith: Arithmetic[SInt, SInt]       = Arithmetic.sIntArithmetic
    implicit val pt:    PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(fabric)

    var allPass = true

    {
      val tiles = golden.shapedTiles
      // Summed across k-tiles in software and checked against the full C once the run
      // ends: the per-tile compare localises a failure, this is what makes the PASS mean
      // the config computes this GEMM.
      val totals = new SimHarness.BlockTotals(golden,
        if (dual) s"$cfgName gemm ${cfg.dataflow} no-acc" else s"Toroidal ${cfg.dataflow} no-acc")

      SimConfig.compile(new ToroidalSystolicArray[SInt, SInt](fabric, dualMode = dual)).doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        val poke = new SimHarness.ToroidalPoker(dut, g)
        // Held for the whole run: stock stream at every MAC, stationary bus passed
        // straight through. Both selects cross one register inside the array, so they are
        // set before the reset is released and never touched again.
        poke.setMode(Schedule.Gemm)

        for (tile <- tiles) {
          // Each tile is validated on its own, so no PE state may carry over. The reset
          // is the CALLER's — that is what makes the phase body reusable.
          poke.reset()
          val captured     = SimHarness.phaseGemm(poke, tile, g.drainTail)
          val streamCycles = poke.streamCyclesOf(tile)

          val where = s"(m=${tile.index.m}, n=${tile.index.n}, k=${tile.index.k})"
          val label = if (dual) s"$cfgName gemm ${cfg.dataflow} no-acc tile $where"
                      else      s"Toroidal ${cfg.dataflow} no-acc tile $where"
          val hit = SimHarness.compareExact(label,
            SimHarness.expectedTileWindow(tile, streamCycles, g.numPorts, cfg.isReuseA), captured)
          allPass &= hit.isDefined
          hit.foreach(d => totals.add(tile, captured, d, streamCycles, g.numPorts, SimHarness.tileMap(cfg.isReuseA)))
        }
      }

      allPass &= totals.check()

      val summary =
        if (dual) s"$cfgName (Toroidal DualMode ${cfg.dataflow}, normal GEMM no-acc)  " +
                  s"${gemm.name} (M=${gemm.M}, N=${gemm.N}, K=${gemm.K}; tiles=${tiles.size})"
        else      s"$cfgName (Toroidal ${cfg.dataflow}, SRAM array no-acc)  ${gemm.name} " +
                  s"(M=${gemm.M}, N=${gemm.N}, K=${gemm.K})"
      println(f"  ${if (allPass) "PASS" else "FAIL"}  $summary")
      return verdictOf(allPass)
    }
  }

  /**
   * The output-indexed panel schedule: the stationary tile is loaded ONCE per K-pass and
   * a whole panel of the free output dimension (M for ReuseB, N for ReuseA) streams
   * through it, accumulating in the AccSram across K before the panel drains as one
   * ping-pong burst. The AccSram's address counter free-runs and wraps at `depth`, so
   * EVERY K-pass drives EXACTLY `depth` psumValid cycles — unused slots stream zeros
   * rather than shortening the burst, which would permanently shift the slot mapping.
   *
   * Drains are captured OVERLAPPED: a finished panel drains from its bank while the next
   * accumulates into the other one, so the capture rides the following panel's ticks
   * instead of a dedicated bubble. The final panel keeps a blocking drain.
   */
  private def toroidalReuseABAcc(setup: Setup): Verdict = {
    import setup._

    implicit val arith: Arithmetic[SInt, SInt]       = Arithmetic.sIntArithmetic
    implicit val pt:    PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(fabric)

    val reuseB = g.reuseB
    var allPass = true

    val depth      = cfg.freeDimensionSize                                  // == AccSram depth
    val slotStride = if (reuseB) golden.tileShape.tileM else golden.tileShape.tileN
    // AccSram's address counter free-runs and wraps at `depth`, so EVERY K-pass must
    // drive EXACTLY `depth` psumValid cycles: a shorter or longer burst permanently
    // shifts the slot-to-output mapping and every later pass accumulates on the wrong
    // addresses.
    if (dual) {
      if (depth % slotStride != 0)
        return skip(cfgName, gemm,
          s"the per-K-pass psumValid burst must equal the AccSram depth ($depth), " +
            s"but the slot stride ($slotStride) does not divide it")
    } else {
      require(depth % slotStride == 0,
        s"[error] psumValid burst per K-pass must equal AccSram depth ($depth), " +
          s"but the slot stride ($slotStride) does not divide it")
    }

    val numSlots      = depth / slotStride
    // Toroidal's PE outputs are immediate, so it reserves no de-skew slot and uses the
    // full panel (Grouped, which does reserve one, keeps its own runners).
    val tilesPerPanel = numSlots
    val pipeTail      = (if (reuseB) cfg.outerPeRow else cfg.outerPeCol) +
      log2Up(cfg.multPerPe) + log2Up(if (reuseB) cfg.localVectorPeRow else cfg.localVectorPeCol) + 2
    // The resident panel drains in `depth` cycles (one address wrap reads every slot
    // once); add the pipe tail and slack for the 2-cycle sync-SRAM read pipeline.
    val drainTail = depth + pipeTail + 8

    // Panels: grouped by the tile index the panel HOLDS FIXED (the stationary operand's
    // free index) plus which block of the swept dimension it covers.
    def panelKey(t: GoldenMatrix.ShapedTile): (Int, Int) =
      if (reuseB) (t.index.n, t.index.m / tilesPerPanel) else (t.index.m, t.index.n / tilesPerPanel)
    /** Index of the dimension a panel sweeps — M for ReuseB, N for ReuseA. */
    def sweptIndex(t: GoldenMatrix.ShapedTile): Int = if (reuseB) t.index.m else t.index.n

    val panels = golden.shapedTiles.groupBy(panelKey).toSeq.sortBy(_._1)

    SimConfig.compile(new ToroidalSystolicArray[SInt, SInt](fabric, dualMode = dual)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val poke = new SimHarness.ToroidalPoker(dut, g)
      poke.setMode(Schedule.Gemm)

      val drain = new SimHarness.OverlappedDrain(poke, capacity = drainTail, numPorts = g.numPorts)
      def tick(): Unit = drain.tick()
      poke.onTick = () => tick()

      poke.reset()

      for (((fixed, panelIdx), tiles) <- panels) {
        val slots  = tiles.map(sweptIndex).distinct.sorted    // <= tilesPerPanel of them
        val ks     = tiles.map(_.index.k).distinct.sorted
        val bySlot = tiles.map(t => (t.index.k, sweptIndex(t)) -> t).toMap

        // No inter-panel reset: consecutive panels alternate banks via the
        // writeSel/drainSel ping-pong, and each K-pass's firstLoop overwrite
        // re-initialises every slot. A reset would force writeSel back to bank 0.
        for ((k, ki) <- ks.zipWithIndex) {
          val lastK = ki == ks.size - 1
          // The stationary tile is loaded once for the whole panel sweep — shared by every
          // slot, so any slot's copy of it will do — and the previous panel's in-flight
          // drain is captured concurrently, because the K-pass body clocks on `onTick`.
          // On the LAST K-pass the top-up happens after the settle and before the first
          // psum cycle: the one point where drainSel still points at the PREVIOUS panel's
          // bank, so it can never capture the wrong one.
          val shared = bySlot((k, slots.head))
          SimHarness.accGemmKPass(
            poke,
            stationary  = if (reuseB) shared.b else shared.a,
            settle      = g.settleGemm,
            slotAt      = slot => slots.lift(slot).map(sv => if (reuseB) bySlot((k, sv)).a else bySlot((k, sv)).b),
            numSlots    = numSlots,
            slotStride  = slotStride,
            firstK      = ki == 0,
            lastK       = lastK,
            beforeBurst = () => if (lastK) allPass &= drain.drainToFull(),
          )
        }

        //3. Register THIS panel as the pending drain — captured during the next panel's
        //   driving, or by the final blocking drain below.
        val label =
          if (dual)        s"$cfgName gemm ${cfg.dataflow} acc panel " +
            (if (reuseB) s"(n=$fixed, m=${slots.mkString("/")})" else s"(m=$fixed, n=${slots.mkString("/")})")
          else if (reuseB) s"Toroidal ReuseB BigSram panel (n=$fixed, mPanel=$panelIdx, m=${slots.mkString("/")})"
          else             s"Toroidal ReuseA BigSram panel (m=$fixed, nPanel=$panelIdx, n=${slots.mkString("/")})"
        drain.register(label,
          SimHarness.expectedPanelWindow(golden, fixed, slots, slotStride, !reuseB), depth)
      }

      //4. The final panel has no successor — drain it as a blocking burst.
      for (_ <- 0 until drainTail) {
        poke.zeroInputs()
        tick()
      }
      allPass &= drain.validate()
    }

    val summary =
      if (dual) s"$cfgName (Toroidal DualMode ${cfg.dataflow}, normal GEMM big SRAM)  " +
                s"${gemm.name} (M=${gemm.M}, N=${gemm.N}, K=${gemm.K}; depth=$depth, " +
                s"slotStride=$slotStride, panels=${panels.size})"
      else      s"$cfgName (Toroidal ${cfg.dataflow}, big SRAM)  ${gemm.name} " +
                s"(M=${gemm.M}, N=${gemm.N}, K=${gemm.K})"
    println(f"  ${if (allPass) "PASS" else "FAIL"}  $summary")
    verdictOf(allPass)
  }

  // ── Toroidal ReuseC: per-tile validation through the SRAM array (no accumulator) ──

  /**
   * Run one tiled GEMM through the Toroidal SRAM-array ReuseC path, comparing each tile's
   * captured drain window against its `partialC` element for element and then checking
   * that the k-tiles sum to C.
   *
   * The DRIVE loop is the same schedule as [[GroupedDriver.runGroupedReuseC]] — only the
   * elaborated DUT and the label differ — but the CHECKING is not: here
   * `systolicOutputDimC == totalVectorPeCol`, so the port axis IS C's column axis and the
   * outputs land every other cycle, while Grouped folds its ports. See
   * [[SimHarness.reuseCMap]].
   *
   * Its contraction bound is the wrap's CLUSTER-loop length, an architectural capacity
   * rather than a provisioning choice, so a K past it is skipped by the dispatcher rather
   * than summed in software.
   */
  def runToroidalReuseC(cfg: SystolicArrayConfig, cfgName: String, gemm: GemmDim): Verdict = {
    require(cfg.isReuseC,         s"[error] runToroidalReuseC expects ReuseC, got ${cfg.dataflow}")
    require(!cfg.needAccumulator, "[error] SRAM ReuseC path needs no accumulator")

    implicit val arith: Arithmetic[SInt, SInt]       = Arithmetic.sIntArithmetic
    implicit val pt:    PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(cfg)

    val golden = new GoldenMatrix(gemm, cfg)

    val topology      = cfg.topology
    val numInputA     = topology.systolicInputDimA
    val numInputB     = topology.systolicInputDimB
    val numOutput     = topology.systolicOutputDimC
    val numAccEnableC = topology.numAccEnableC

    val tiles     = golden.shapedTiles
    // The top PE row (r=0) drains last: its partial sum pulses out ~outerPeRow cycles
    // after streaming ends, then propagates DOWN the C-chain (2 register stages per row)
    // to the bottom output. Worst-case last-output latency is ~3*outerPeRow; the earlier
    // 2*(P+Q)-2 bound is one cycle short for the smallest (2x2) arrays and clips that row.
    // A window longer than the drain is harmless: the exact compare requires the extra
    // cycles to be ZERO, which the capture dumps show they already are.
    val drainTail = 3 * (cfg.totalVectorPeRow + cfg.totalVectorPeCol) + 2 +
      log2Up(cfg.multPerPe) + log2Up(math.max(cfg.localVectorPeRow, cfg.localVectorPeCol))

    // The (drain step, output port) -> (row, col) mapping this fabric's C-chain wires,
    // used BOTH to build the expected window and to sum the k-tiles.
    val map      = SimHarness.reuseCMap(cfg)
    val mapSteps = SimHarness.reuseCSteps(cfg)
    val totals   = new SimHarness.BlockTotals(golden, s"Toroidal ReuseC")

    var allPass = true

    SimConfig.compile(new ToroidalSystolicArray[SInt, SInt](cfg)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      val poke = new SimHarness.StockPoker(dut, cfg)
      poke.reset()

      for ((tile, tileIdx) <- tiles.zipWithIndex) {
        val shapedA       = tile.a
        val shapedB       = tile.b
        val streamCycles  = shapedA.cols
        val captureCycles = streamCycles + drainTail

        val outputs = Array.fill(captureCycles, numOutput)(BigInt(0))

        if (tileIdx > 0) poke.reset()

        for (t <- 0 until captureCycles) {
          if (t < streamCycles) {
            //1. Stream Tile A and Tile B
            for (k <- 0 until numInputA)
              dut.io.inputA(k) #= (if (k < shapedA.rows) shapedA(k, t) else 0)
            val brow = if (t < shapedB.rows) shapedB.row(t) else Array.empty[Int]
            for (k <- 0 until numInputB)
              dut.io.inputB(k) #= (if (k < brow.length) brow(k) else 0)
            for (i <- 0 until numAccEnableC) dut.io.inputAccEnableC(i) #= true
          } else {
            //2. Flush outputs
            poke.zeroInputs()
          }

          dut.clockDomain.waitRisingEdge()
          for (i <- 0 until numOutput) outputs(t)(i) = dut.io.outputC(i).toBigInt
        }

        //3. Validate the drain window against partialC, element for element: every value
        //   pinned to a (cycle, port) pair, and every cycle outside the window required
        //   to be zero. The bag compare this replaces discarded order and port position,
        //   so a permuted port mapping or a rotated drain passed it.
        val label = s"Toroidal ReuseC BigSram tile (m=${tile.index.m}, n=${tile.index.n}, k=${tile.index.k})"
        val hit = SimHarness.compareExact(label, SimHarness.expectedReuseCWindow(cfg, tile), outputs)
        allPass &= hit.isDefined
        hit.foreach(d => totals.add(tile, outputs, d, mapSteps, numOutput, map))
      }
    }
    allPass &= totals.check()

    println(f"  ${if (allPass) "PASS" else "FAIL"}  $cfgName (Toroidal ReuseC, SRAM array)  ${gemm.name} " +
      s"(M=${gemm.M}, N=${gemm.N}, K=${gemm.K})")
    verdictOf(allPass)
  }


}
