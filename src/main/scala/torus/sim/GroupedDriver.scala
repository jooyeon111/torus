package torus.sim

import spinal.core._
import spinal.core.sim._

import torus.arithmetic._
import torus.array._
import torus.config._

/**
 * Every runner that drives a [[GroupedSystolicArray]].
 *
 * Grouped is the half of the stock schedule that will NOT merge into Toroidal: it
 * streams the TRANSPOSED tile and reserves the last accumulator slot of each panel as a
 * TypeC de-skew zero-flush, so it is a different schedule rather than a mirror. A file
 * boundary on the seam that is known not to close is a cheap way to stop the next reader
 * re-attempting it.
 *
 * The bodies here are the ones [[DataflowDriver]] dispatched to before the split; the
 * dispatcher keeps only the key, the guards and the labelling.
 */
object GroupedDriver {

  /** One `[skip]` line, and the verdict that carries its reason. */
  private def skip(cfgName: String, gemm: GemmDim, why: String): Verdict = {
    println(s"  [skip] $cfgName × ${gemm.name} — $why")
    Verdict.Skip(why)
  }

  private def verdictOf(ok: Boolean): Verdict = DataflowDriver.verdictOf(ok)

  // ── Grouped ReuseA / ReuseB + big SRAM: one body for the mirror pair ──

  /**
   * Run one tiled GEMM through the Grouped SRAM-accumulator array using the
   * output-indexed panel dataflow, for BOTH ReuseA and ReuseB.
   *
   * The stationary tile is loaded ONCE per K-pass and a whole panel of the free output
   * dimension streams through it, accumulating in the AccSram across K before the panel
   * drains as one ping-pong burst:
   *
   *   - ReuseA holds A(m,k) and sweeps an N-PANEL (fixed m, up to `tilesPerPanel`
   *     N-tiles sharing that A) across all K;
   *   - ReuseB holds B(k,n) and sweeps an M-PANEL, the mirror.
   *
   * Which operand is stationary is a PARAMETER of this body — [[SimHarness.GroupedGeometry]]
   * states the mirror once — not a routing decision. The two runners this replaces had no
   * structural difference at all: every differing line was the substitution A<->B,
   * m<->n, row<->col, down to their local `inputBValue` / `inputAValue` helpers.
   *
   * AccSram's address counter free-runs and wraps at `depth`, so EVERY K-pass drives
   * EXACTLY `depth` psumValid cycles: a shorter or longer burst permanently shifts the
   * slot-to-output mapping and every later pass accumulates on the wrong addresses.
   * Unused slots and short rows stream zeros rather than shortening the burst.
   *
   * Grouped aligns its output ports via its TypeC de-skew buffer + [[AccTap]], so the
   * shared-address big SRAM is valid (see `attachAccumulatorIfNeeded`). It reserves the
   * LAST slot of each panel (`tilesPerPanel = numSlots - 1`) as a de-skew zero-flush: its
   * final real rows are still walking the de-skew chain when the burst ends, and only
   * clock into the accumulator while that trailing slot streams zeros.
   */
  def runGroupedReuseABAcc(cfg: SystolicArrayConfig, cfgName: String, gemm: GemmDim): Verdict = {
    require(cfg.isGrouped,       s"[error] runGroupedReuseABAcc expects a Grouped config, got ${cfg.fabric}")
    require(cfg.isReuseAB,       s"[error] runGroupedReuseABAcc expects ReuseA/ReuseB, got ${cfg.dataflow}")
    require(cfg.needAccumulator, "[error] big-SRAM path requires needAccumulator")

    implicit val arith: Arithmetic[SInt, SInt]       = Arithmetic.sIntArithmetic
    implicit val pt:    PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(cfg)

    val golden = new GoldenMatrix(gemm, cfg)
    val gg     = new SimHarness.GroupedGeometry(cfg)

    val depth      = cfg.freeDimensionSize                 // == AccSram depth
    val slotStride = gg.slotStrideOf(golden.tileShape)     // accumulator slots per swept tile
    require(depth % slotStride == 0,
      s"[error] psumValid burst per K-pass must equal AccSram depth ($depth), " +
        s"but the slot stride ($slotStride) does not divide it")
    val numSlots   = depth / slotStride
    // Reserve the last slot of each panel as a de-skew zero-flush (see scaladoc).
    val tilesPerPanel = numSlots - 1
    require(tilesPerPanel >= 1,
      s"[error] Grouped reserves one of the $numSlots accumulator slots as the de-skew " +
        s"zero-flush, so freeDimensionSize ($depth) must hold at least two slots")
    // The resident panel drains in `depth` cycles (one address wrap reads every slot
    // once); the ping-pong then re-reads the now-zeroed buffer, so capturing past
    // `depth` only yields zeros. Add the pipe tail + slack to flush the 2-cycle
    // sync-SRAM read pipeline.
    val drainTail = depth + gg.pipeTail + 8

    // Panels: grouped by the index the panel HOLDS FIXED (the stationary operand's free
    // index) plus which block of the swept dimension it covers. The slot loop still spans
    // every slot, so the reserved trailing slot streams zeros.
    val panels = golden.shapedTiles.groupBy(t => gg.panelKey(t, tilesPerPanel)).toSeq.sortBy(_._1)

    var allPass = true

    SimConfig.compile(new GroupedSystolicArray[SInt, SInt](cfg)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      val poke = new SimHarness.StockPoker(dut, cfg)
      poke.reset()

      // ── Overlapped drain capture ────────────────────────────────────────────
      // A finished panel drains from its bank while the NEXT panel accumulates into the
      // other one (the AccSramDoubleBuffer ping-pong), so we capture a panel's drain on
      // every clock tick of the following panel's weight-load + stream instead of paying
      // a dedicated `drainTail` bubble. The final panel keeps a blocking drain.
      val drain = new SimHarness.OverlappedDrain(poke, capacity = drainTail, numPorts = gg.numPorts)
      def tick(): Unit = drain.tick()
      poke.onTick = () => tick()

      for (((fixed, panelIdx), tiles) <- panels) {
        val slots  = tiles.map(gg.sweptIndex).distinct.sorted    // <= tilesPerPanel of them
        val ks     = tiles.map(_.index.k).distinct.sorted
        val bySlot = tiles.map(t => (t.index.k, gg.sweptIndex(t)) -> t).toMap

        // No inter-panel poke.reset(): consecutive panels alternate banks via the
        // writeSel/drainSel ping-pong and each K-pass's firstLoop overwrite
        // re-initialises every slot, so a reset is unnecessary — and would force writeSel
        // back to bank 0, defeating the ping-pong. Only the one initial reset remains.
        for ((k, ki) <- ks.zipWithIndex) {
          val lastK = ki == ks.size - 1
          // The stationary tile is loaded once for the whole panel sweep, and the previous
          // panel's in-flight drain is captured concurrently because the K-pass body
          // clocks on `onTick`. On the LAST K-pass the top-up happens after the settle and
          // before the first psum cycle — while drainSel still points at the PREVIOUS
          // panel's bank, so it can never capture the wrong one. The reserved de-skew slot
          // streams zeros like any other unused one.
          val shared = bySlot((k, slots.head))
          SimHarness.accGemmKPass(
            poke,
            stationary  = if (gg.reuseA) shared.a else shared.b,
            settle      = gg.settleGemm,
            slotAt      = slot => slots.lift(slot).map(sv => if (gg.reuseA) bySlot((k, sv)).b else bySlot((k, sv)).a),
            numSlots    = numSlots,
            slotStride  = slotStride,
            firstK      = ki == 0,
            lastK       = lastK,
            beforeBurst = () => if (lastK) allPass &= drain.drainToFull(),
          )
        }

        //3. Register THIS panel as the pending drain — captured during the next panel's
        //   driving, or by the final blocking drain below. The reserved de-skew slot is
        //   part of the expected window (it drains zero), not an exception to it.
        val label =
          if (gg.reuseA) s"Grouped ReuseA BigSram panel (m=$fixed, nPanel=$panelIdx, n=${slots.mkString("/")})"
          else           s"Grouped ReuseB BigSram panel (n=$fixed, mPanel=$panelIdx, m=${slots.mkString("/")})"
        drain.register(label,
          SimHarness.expectedPanelWindow(golden, fixed, slots, slotStride, gg.reuseA), depth)
      }

      //4. The final panel has no successor — drain it as a blocking burst.
      for (_ <- 0 until drainTail) {
        poke.zeroInputs()
        tick()
      }
      allPass &= drain.validate()
    }

    println(f"  ${if (allPass) "PASS" else "FAIL"}  $cfgName (Grouped ${cfg.dataflow}, big SRAM)  ${gemm.name} " +
      s"(M=${gemm.M}, N=${gemm.N}, K=${gemm.K})")
    verdictOf(allPass)
  }

  // ── Grouped ReuseC: per-tile validation through the SRAM array (no accumulator) ──

  /**
   * Run one tiled GEMM through the Grouped SRAM-array ReuseC path and compare each
   * tile's captured drain window against its `partialC` element for element, then check
   * that the k-tiles sum to C.
   *
   * ReuseC needs no accumulator, so the big SRAM is never attached; this exercises the
   * `GroupedSystolicArray` ReuseC fall-back wiring. The drain FOLDS on this fabric once
   * `localVectorPeRow > 1` — `outerPeRow` cycles of `outerPeCol * lvR * lvC` ports, each
   * cycle carrying `lvR` row-slices at once — which is what
   * [[SimHarness.reuseCMap]] decomposes.
   */
  def runGroupedReuseC(cfg: SystolicArrayConfig, cfgName: String, gemm: GemmDim): Verdict = {
    require(cfg.isReuseC,         s"[error] runGroupedReuseC expects ReuseC, got ${cfg.dataflow}")
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
    val totals   = new SimHarness.BlockTotals(golden, s"Grouped ReuseC")

    var allPass = true

    SimConfig.compile(new GroupedSystolicArray[SInt, SInt](cfg)).doSim { dut =>
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
        val label = s"Grouped ReuseC BigSram tile (m=${tile.index.m}, n=${tile.index.n}, k=${tile.index.k})"
        val hit = SimHarness.compareExact(label, SimHarness.expectedReuseCWindow(cfg, tile), outputs)
        allPass &= hit.isDefined
        hit.foreach(d => totals.add(tile, outputs, d, mapSteps, numOutput, map))
      }
    }
    allPass &= totals.check()

    println(f"  ${if (allPass) "PASS" else "FAIL"}  $cfgName (Grouped ReuseC, SRAM array)  ${gemm.name} " +
      s"(M=${gemm.M}, N=${gemm.N}, K=${gemm.K})")
    verdictOf(allPass)
  }

  // ── ReuseA / ReuseB WITHOUT accumulator: per-tile validation ───────────────

  /**
   * Run one tiled GEMM through the Grouped SRAM array with `needAccumulator = false`.
   *
   * The Toroidal half of this runner lives in [[ToroidalDriver.runToroidalReuseAB]], which
   * drives the same fabric through the same schedule as the accumulator path and the
   * dual-mode wrapper. Grouped keeps its own body: it streams the TRANSPOSED tile and its
   * de-skew buffer changes the output alignment, so it is a different schedule, not a
   * mirror.
   *
   * With no accumulator the array attaches no big SRAM; its ReuseA/ReuseB output is the
   * de-skewed per-cycle partial-sum vector straight from `postProcessorC` — the
   * `outputC := collected` fall-back in [[SystolicArray.attachAccumulatorIfNeeded]].
   *
   * The big SRAM only ever sums ACROSS K-passes into the same slot, never the cycles
   * WITHIN one K-tile, so each streamed cycle already carries a complete, distinct element
   * of that tile's `partialC`: step `t` port `q` IS C[t][q] for ReuseB and C[q][t] for
   * ReuseA ([[SimHarness.tileMap]]). Every shaped tile is checked on its own — which is
   * what localises a failure — and then the captured k-tiles are summed in software and
   * compared against the full C, which is what makes the PASS mean this config computes
   * this GEMM.
   */
  def runGroupedReuseABNoAcc(cfg: SystolicArrayConfig, cfgName: String, gemm: GemmDim): Verdict = {
    require(cfg.isGrouped, s"[error] runGroupedReuseABNoAcc expects a Grouped config, got ${cfg.fabric}")
    require(cfg.isReuseAB,        s"[error] runGroupedReuseABNoAcc expects ReuseA/ReuseB, got ${cfg.dataflow}")
    require(!cfg.needAccumulator, "[error] runGroupedReuseABNoAcc is the no-accumulator path")

    implicit val arith: Arithmetic[SInt, SInt]       = Arithmetic.sIntArithmetic
    implicit val pt:    PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(cfg)

    val golden = new GoldenMatrix(gemm, cfg)

    val topology  = cfg.topology
    val numInputA = topology.systolicInputDimA
    val numInputB = topology.systolicInputDimB
    val numOutput = topology.systolicOutputDimC
    val reuseA    = cfg.isReuseA

    // How many steps it takes to stream ONE tile's moving operand. With no
    // accumulator there is no address stride, so this is a plain duration — the
    // shaped tile's REAL extent along the free dimension, not a config field.
    // (Grouped streams the TRANSPOSED tile, hence the rows/cols swap against the
    // Toroidal runner's.)
    def streamCyclesOf(tile: GoldenMatrix.ShapedTile): Int =
      if (reuseA) tile.b.rows else tile.a.cols
    // Generous drain window: worst-case last-output latency through the de-skew +
    // C-chain, matching the ReuseC bound.
    val drainTail = 3 * (cfg.totalVectorPeRow + cfg.totalVectorPeCol) + 2 +
      log2Up(cfg.multPerPe) + log2Up(math.max(cfg.localVectorPeRow, cfg.localVectorPeCol))

    // Streamed-operand element at (port, stream-step t): Grouped streams the transposed
    // tile's columns.
    def streamB(b: Matrix, port: Int, t: Int): Int = {
      val r = if (t < b.rows) b.row(t) else Array.empty[Int]
      if (port < r.length) r(port) else 0
    }
    def streamA(a: Matrix, port: Int, t: Int): Int =
      if (t < a.cols && port < a.rows) a(port, t) else 0

    val tiles = golden.shapedTiles
    var allPass = true
    // Summed across k-tiles in software and checked against the full C once the run ends:
    // the per-tile compare localises a failure, this is what makes the PASS mean the
    // config computes this GEMM.
    val totals = new SimHarness.BlockTotals(golden, s"Grouped ${cfg.dataflow} no-acc")

    def doTest(dut: Component with SystolicArray[SInt, SInt]): Unit = {
      dut.clockDomain.forkStimulus(10)

      val poke = new SimHarness.StockPoker(dut, cfg)
      poke.reset()

      for ((tile, tileIdx) <- tiles.zipWithIndex) {
        // Reset between tiles: without an accumulator each tile is validated on its
        // own, so PE/de-skew state must not carry over.
        if (tileIdx > 0) poke.reset()

        val shapedA = tile.a
        val shapedB = tile.b

        //1. Pre-load the stationary operand (A for ReuseA, B for ReuseB).
        if (reuseA) {
          for (col <- 0 until shapedA.cols) {
            for (p <- 0 until numInputA) dut.io.inputA(p) #= (if (p < shapedA.rows) shapedA(p, col) else 0)
            poke.update(true)
            dut.clockDomain.waitRisingEdge()
          }
        } else {
          for (rowIdx <- 0 until shapedB.rows) {
            val r = shapedB.row(rowIdx)
            for (p <- 0 until numInputB) dut.io.inputB(p) #= (if (p < r.length) r(p) else 0)
            poke.update(true)
            dut.clockDomain.waitRisingEdge()
          }
        }
        poke.update(false)
        for (i <- 0 until numInputA) dut.io.inputA(i) #= 0
        for (i <- 0 until numInputB) dut.io.inputB(i) #= 0
        // Weight-load settle: ReuseA fills columns (totalVectorPeCol depth),
        // ReuseB fills rows (totalVectorPeRow depth); minus the outerPe hops the
        // streamed operand's own propagation overlaps.
        val settle = if (reuseA) cfg.totalVectorPeCol - cfg.outerPeCol
                     else        cfg.totalVectorPeRow - cfg.outerPeRow
        for (_ <- 0 until settle) dut.clockDomain.waitRisingEdge()

        //2. Stream the moving operand and capture every output cycle.
        val streamCycles  = streamCyclesOf(tile)
        val captureCycles = streamCycles + drainTail
        val outputs = Array.fill(captureCycles, numOutput)(BigInt(0))
        for (t <- 0 until captureCycles) {
          if (t < streamCycles) {
            if (reuseA)
              for (row <- 0 until numInputB) dut.io.inputB(row) #= streamB(shapedB, row, t)
            else
              for (col <- 0 until numInputA) dut.io.inputA(col) #= streamA(shapedA, col, t)
          } else {
            for (i <- 0 until numInputA) dut.io.inputA(i) #= 0
            for (i <- 0 until numInputB) dut.io.inputB(i) #= 0
          }
          dut.clockDomain.waitRisingEdge()
          for (i <- 0 until numOutput) outputs(t)(i) = dut.io.outputC(i).toBigInt
        }

        //3. Exact per-cycle compare against the tile's partialC.
        val label = s"Grouped ${cfg.dataflow} no-acc tile (m=${tile.index.m}, n=${tile.index.n}, k=${tile.index.k})"
        val hit = SimHarness.compareExact(label,
          SimHarness.expectedTileWindow(tile, streamCycles, numOutput, reuseA), outputs)
        allPass &= hit.isDefined
        hit.foreach(d => totals.add(tile, outputs, d, streamCycles, numOutput, SimHarness.tileMap(reuseA)))
      }
    }

    SimConfig.compile(new GroupedSystolicArray[SInt, SInt](cfg)).doSim(doTest _)
    allPass &= totals.check()

    println(f"  ${if (allPass) "PASS" else "FAIL"}  $cfgName (Grouped ${cfg.dataflow}, SRAM array no-acc)  ${gemm.name} " +
      s"(M=${gemm.M}, N=${gemm.N}, K=${gemm.K})")
    verdictOf(allPass)
  }

}
