package torus.sim

import spinal.core._
import spinal.core.sim._

import torus.array._
import torus.config._
import torus.core._

/**
 * The pieces every simulation runner needs, in one place: the output comparator, the
 * dual-mode array's geometry and stimulus helpers, the stock arrays' stimulus helpers,
 * and the overlapped drain capture.
 *
 * Before the harness merge each runner carried its own copy — three bag comparators,
 * two second-stream `lanes` mappings, three dual-mode `Poker`s, four sets of
 * `driveAcc`/`zeroInputs`/`resetArray` closures. The `lanes` duplication was the
 * actively dangerous one: it encodes the top-level bus ordering that
 * `ToroidalSystolicArray.wireDualModeA/B` wires, so an RTL change to that ordering
 * needed two edits and editing only one left a test that still passed.
 */
object SimHarness {

  // ════════════════════════════════════════════════════════════
  //  Comparators
  // ════════════════════════════════════════════════════════════

  /**
   * Exact compare: find the capture offset at which the WHOLE expected window matches
   * element for element, and require every cycle outside that window to be all-zero.
   *
   * This is the check the bag compare it replaced could not make. A multiset compare
   * recovers multiplicity but still discards ORDER and PORT POSITION, so a permuted port
   * mapping, a rotated slot order and a latency regression inside the drain window all
   * passed it; pinning every element to a (cycle, port) pair does not. `compareBag` is
   * DELETED rather than left unused — a live function with no callers is an invitation
   * for the next runner to reach for it. The offset itself is not asserted — it is an implementation detail of the
   * pipeline depth — but it is REPORTED, so a latency change shows up as a changed
   * number rather than as silence.
   *
   * Only usable where the capture window is deterministic: the run must start from
   * reset, or the offset search has nothing stable to lock onto.
   *
   * Returns the offset it locked onto, or None if the capture does not match. Callers
   * that only want a verdict read `.isDefined`; the ones that go on to SUM the captured
   * window across k-tiles need the offset to find the window again.
   */
  def compareExact(
    label:    String,
    expected: Array[Array[Int]],
    captured: Array[Array[BigInt]],
  ): Option[Int] = {
    val steps = expected.length
    val ports = if (steps == 0) 0 else expected(0).length

    val hit = (0 to captured.length - steps).find { d =>
      (0 until steps).forall(t => (0 until ports).forall(q => captured(d + t)(q) == BigInt(expected(t)(q))))
    }

    hit match {
      case None =>
        println(s"      [fail] $label: no capture offset matches the reference C")
        val shown = captured.indices.filter(i => captured(i).exists(_ != 0)).take(steps + 4)
        for (t <- 0 until math.min(steps, 4)) println(s"        want step $t: ${expected(t).mkString(", ")}")
        shown.foreach(i => println(f"        got  cycle $i%3d: ${captured(i).mkString(", ")}"))
        None

      case Some(d) =>
        // Nothing may appear outside the aligned window: a leaking pipeline, a second
        // spurious emission or a latency regression inside the drain tail all show here.
        val stray = captured.indices.filter(t => t < d || t >= d + steps).filter(t => captured(t).exists(_ != 0))
        if (stray.nonEmpty) {
          println(s"      [fail] $label: non-zero output outside the aligned window at cycles ${stray.take(8).mkString(", ")}")
          stray.take(3).foreach(i => println(f"        cycle $i%3d: ${captured(i).mkString(", ")}"))
          None
        } else {
          println(s"      [pass] $label: ${steps * ports} outputs exact, latency $d")
          Some(d)
        }
    }
  }

  /**
   * Exact compare of a DRAINED ACCUMULATOR PANEL: search for the (offset, rotation) pair
   * at which the whole panel matches element for element, then require every cycle
   * outside that window to be quiet.
   *
   * A drain differs from a stream window in two ways a plain [[compareExact]] gets wrong:
   *
   *  - the free-running address counter decides WHICH SLOT drains first, so the search is
   *    over an (offset, ROTATION) pair rather than an offset alone. The rotation is not
   *    asserted — which slot comes out first is an implementation detail — but it IS
   *    reported, so a rotation that stops being 0 is a finding worth reading;
   *  - outside the window "quiet" means NO NEW VALUE APPEARS, not "zero". `outputC` is a
   *    `RegNextWhen` on `outputValid` and HOLDS its last word between drains, and the
   *    drain stays active one cycle past the address wrap (`drainActive = burstEnded ||
   *    draining`) and re-reads a slot the read-and-zero pass has already cleared. Both are
   *    legitimate on correct hardware, so a zeros-outside rule would fail it. Understand
   *    both before tightening this.
   *
   * `want(slot, port)` is the value accumulator slot `slot` should hold on output port
   * `port`; [[expectedPanelWindow]] builds it for the stock-GEMM panel schedule.
   *
   * This is `DualModeDriver.checkDrain` made generic in `want` and moved here, so the
   * GEMV panel check and the two stock-GEMM ones are one comparator rather than three.
   */
  def compareExactDrain(
    label:    String,
    want:     (Int, Int) => BigInt,
    depth:    Int,
    numPorts: Int,
    captured: Array[Array[BigInt]],
  ): Option[(Int, Int)] = {
    val hit = (for {
      d   <- 0 to captured.length - depth
      rho <- 0 until depth
      if (0 until depth).forall(i => (0 until numPorts).forall(q => captured(d + i)(q) == want((i + rho) % depth, q)))
    } yield (d, rho)).headOption

    hit match {
      case None =>
        println(s"      [fail] $label: no drain window matches the reference C")
        println(s"        want (slot-major): ${(0 until depth).map(sl => (0 until numPorts).map(q => want(sl, q)).mkString("[", ",", "]")).mkString(" ")}")
        val shown = captured.indices.filter(i => captured(i).exists(_ != 0)).take(depth + 4)
        shown.foreach(i => println(f"        cycle $i%3d: ${captured(i).mkString(", ")}"))
        None

      case Some((d, rho)) =>
        def quiet(i: Int): Boolean =
          captured(i).forall(_ == 0) || (i > 0 && captured(i).sameElements(captured(i - 1)))
        val noisy = captured.indices.filter(i => i < d || i >= d + depth).filterNot(quiet)
        if (noisy.nonEmpty) {
          println(s"      [fail] $label: a new non-zero word appears outside the drain window " +
            s"(offset $d) at cycles ${noisy.take(8).mkString(", ")}")
          noisy.take(3).foreach(i => println(f"        cycle $i%3d: ${captured(i).mkString(", ")}"))
          None
        } else {
          println(s"      [pass] $label: ${depth * numPorts} accumulated outputs exact " +
            s"(drain offset $d, slot rotation $rho)")
          Some((d, rho))
        }
    }
  }

  /**
   * Which C element each (accumulator slot, output port) pair of a drained stock-GEMM
   * panel carries — the `want` [[compareExactDrain]] is given.
   *
   * A panel holds `depth` slots of `numPorts` values, and the slot layout is fixed by the
   * driver that wrote it: the same `(slot, slotStride)` decomposition the psum burst uses.
   * Slot `addr / slotStride` says which SWEPT tile the address belongs to and
   * `addr % slotStride` says which row inside it.
   *
   * Three facts make this exact rather than approximate:
   *
   *  - the burst length is fixed at `depth`: every K-pass drives exactly `depth`
   *    psumValid cycles and unused slots stream ZEROS rather than shortening the burst,
   *    so the address-to-output mapping is stable across passes and panels;
   *  - Grouped's reserved de-skew slot is part of the mapping, not an exception. It
   *    reserves the last slot of each panel, so `slots.lift` returns None there and the
   *    expected word is a zero — and the de-skew itself does not perturb the mapping,
   *    since [[AccTapCell]] delays each cell's CONTROL to match its data. That is why one
   *    builder serves both fabrics;
   *  - the rotation is searched, not asserted.
   *
   * `fixed` is the index the panel holds fixed (n for ReuseB, m for ReuseA) and `slots`
   * the swept indices it covers, in slot order.
   */
  def expectedPanelWindow(
    golden:     GoldenMatrix,
    fixed:      Int,
    slots:      Seq[Int],
    slotStride: Int,
    reuseA:     Boolean,
  ): (Int, Int) => BigInt = {
    // referenceCBlock re-multiplies, so memoise per slot rather than per (addr, port).
    val blocks = slots.map { s =>
      val (m, n) = if (reuseA) (fixed, s) else (s, fixed)
      golden.referenceCBlock(m, n)
    }
    (addr: Int, port: Int) => {
      val slot = addr / slotStride
      val row  = addr % slotStride
      blocks.lift(slot) match {
        case None => BigInt(0)                       // unused / reserved slot: streamed zeros, drains zero
        case Some(block) =>
          val (r, c) = if (reuseA) (port, row) else (row, port)
          if (r < block.rows && c < block.cols) BigInt(block(r, c)) else BigInt(0)
      }
    }
  }

  /**
   * Which (row, col) of a tile's C each (capture step, output port) pair carries.
   *
   * ONE definition serves both the expected window [[compareExact]] is given and the
   * cross-k-tile summation [[BlockTotals]] performs, so the two cannot drift: a mapping
   * bug would otherwise make the per-tile check fail and the totals check "agree" with it.
   * `(-1, -1)` means the step carries nothing at that port (an interleaved zero cycle).
   */
  type OutputMap = (Int, Int) => (Int, Int)

  /**
   * The no-accumulator ReuseA/ReuseB tile mapping.
   *
   * ReuseB streams the A rows, so the step IS m and the port IS n; ReuseA is the mirror
   * and streams the B columns, so the step is n and the port is m. Every fabric agrees:
   * Grouped's de-skew buffer puts its ports back in step, which is the same property that
   * makes its shared-address accumulator legal at all, and the dual-mode wrapper in
   * stock-GEMM mode simply IS the Toroidal array underneath it.
   */
  def tileMap(reuseA: Boolean): OutputMap =
    (t, q) => if (reuseA) (q, t) else (t, q)

  /**
   * The ReuseC drain mapping, per fabric — the one piece of the expected-window
   * construction that is more than a builder.
   *
   * ══ Toroidal ══ `systolicOutputDimC = totalVectorPeCol`, so the PORT axis IS C's
   * column axis and the CYCLE axis is its row axis, for every broadcast shape. The only
   * difference from a ReuseA/ReuseB window is the STRIDE: outputs land every other cycle
   * (two register stages per row hop on the C-chain), so row `t` sits at step
   * `t * ReuseCStride` and the interleaved steps carry nothing. Including them in the
   * window makes those zeros CHECKED rather than ignored.
   *
   * ══ Grouped ══ `systolicOutputDimC = outerPeCol * numVectorPePerGroupPe`, i.e.
   * `outerPeCol * lvR * lvC` — 64 ports for the sub-array config, not the 16 columns of
   * C. So a 16x16 C comes out in `outerPeRow` cycles of 64 ports, each cycle carrying
   * `lvR` row-slices at once. The port index decomposes the way `SystolicArray`'s
   * `collectReuseCOutputs` wires it — outer column `c`, then the in-group index
   * `idx = a * lvC + b` `GroupPE` uses — giving
   *
   *     n = c * lvC + b        m = (drain row) * lvR + a
   *
   * At `lvR == lvC == 1` this degenerates to `cycle = row, port = col`, which is exactly
   * why the scalar config alone was not enough to see the fold.
   *
   * ══ Row order ══ BOTTOM-UP on both fabrics: C's LAST row comes out first. That is what
   * the C-chain says — it flows from PE row 0 downward and exits at the bottom row, so the
   * bottom row's own partial sum is inserted at the drain pulse and appears first while
   * the rows above it are still walking down — and it is what the capture shows (a
   * top-down window matches no offset on either fabric, and its dump is the reverse of
   * the expected one, cycle for cycle).
   *
   * It is a FIXED layout, measured once: a runner that searched both orders would stop
   * distinguishing them, which is the whole point of an exact compare.
   */
  val ReuseCStride: Int = 2

  /** Whether a ReuseC drain emits C's top row first. Fixed, never searched — see above. */
  val ReuseCRowTopDown: Boolean = false

  def reuseCSteps(cfg: SystolicArrayConfig): Int =
    if (cfg.isGrouped) cfg.outerPeRow
    else               (cfg.totalVectorPeRow - 1) * ReuseCStride + 1

  def reuseCMap(cfg: SystolicArrayConfig): OutputMap = {
    val rows = if (cfg.isGrouped) cfg.outerPeRow else cfg.totalVectorPeRow
    def rowAt(i: Int): Int = if (ReuseCRowTopDown) i else rows - 1 - i
    if (cfg.isGrouped) {
      val lvR = cfg.localVectorPeRow
      val lvC = cfg.localVectorPeCol
      val perGroup = lvR * lvC
      (t, p) => {
        val idx = p % perGroup
        ((rowAt(t) * lvR) + idx / lvC, (p / perGroup) * lvC + idx % lvC)
      }
    } else
      (t, q) => if (t % ReuseCStride != 0) (-1, -1) else (rowAt(t / ReuseCStride), q)
  }

  /**
   * The expected window a capture is compared against: every (step, port) resolved
   * through `map` into the tile's `partialC`.
   *
   * A shaped tile can be padded past the natural tile's extent, so a step or port beyond
   * `partialC` expects a ZERO rather than being skipped — which is what makes the padding
   * cycles part of the check instead of a blind spot.
   */
  def windowOf(map: OutputMap, steps: Int, numPorts: Int, c: Matrix): Array[Array[Int]] =
    Array.tabulate(steps, numPorts) { (t, q) =>
      val (row, col) = map(t, q)
      if (row >= 0 && row < c.rows && col >= 0 && col < c.cols) c(row, col) else 0
    }

  def expectedTileWindow(
    tile:         GoldenMatrix.ShapedTile,
    streamCycles: Int,
    numPorts:     Int,
    reuseA:       Boolean,
  ): Array[Array[Int]] =
    windowOf(tileMap(reuseA), streamCycles, numPorts, tile.partialC)

  /** The expected ReuseC drain window for one tile, per [[reuseCMap]]. */
  def expectedReuseCWindow(cfg: SystolicArrayConfig, tile: GoldenMatrix.ShapedTile): Array[Array[Int]] =
    windowOf(reuseCMap(cfg), reuseCSteps(cfg), cfg.topology.systolicOutputDimC, tile.partialC)

  /**
   * The end-to-end check a no-accumulator run needs on top of its per-tile one: sum the
   * CAPTURED per-k-tile windows in software and compare each output block's total
   * against the full C.
   *
   * The per-tile compare localises a failure; this is what makes a PASS mean "this config
   * computes this GEMM". Without it a no-acc array whose K spans several contraction
   * passes reports a green run for a GEMM it cannot complete: every partial is right and
   * nothing ever asks whether they sum to C.
   * It is the treatment [[DualModeDriver.runGemvNoAcc]] already had, given to the stock
   * runners.
   *
   * The values summed are the ones the DUT produced, at the offset [[compareExact]]
   * locked onto — never the golden partials, which would make the check tautological.
   */
  final class BlockTotals(golden: GoldenMatrix, label: String) {
    private val totals = scala.collection.mutable.Map.empty[(Int, Int), Array[Array[BigInt]]]

    /**
     * Fold one tile's captured window into its (m, n) block.
     *
     * `map` must be the SAME [[OutputMap]] the expected window was built from — that is
     * the point of there being one definition of it.
     */
    def add(
      tile:     GoldenMatrix.ShapedTile,
      captured: Array[Array[BigInt]],
      offset:   Int,
      steps:    Int,
      numPorts: Int,
      map:      OutputMap,
    ): Unit = {
      val c   = tile.partialC
      val key = (tile.index.m, tile.index.n)
      val acc = totals.getOrElseUpdate(key, Array.fill(c.rows, c.cols)(BigInt(0)))
      for (t <- 0 until steps; q <- 0 until numPorts) {
        val (row, col) = map(t, q)
        if (row >= 0 && row < c.rows && col >= 0 && col < c.cols) acc(row)(col) += captured(offset + t)(q)
      }
    }

    /** Every block summed so far against `referenceCBlock`. True when all match. */
    def check(): Boolean = {
      var ok = true
      for (((m, n), got) <- totals.toSeq.sortBy(_._1)) {
        val want  = golden.referenceCBlock(m, n)
        val wrong = for {
          r <- 0 until got.length
          c <- 0 until got(r).length
          if got(r)(c) != BigInt(want(r, c))
        } yield (r, c)
        val where = s"$label k-tiles summed (m=$m, n=$n)"
        if (wrong.isEmpty)
          println(s"      [pass] $where == reference C (${got.length * got.head.length} elements)")
        else {
          ok = false
          println(s"      [fail] $where != reference C at ${wrong.take(8).map { case (r, c) => s"($r,$c)" }.mkString(", ")}" +
            (if (wrong.length > 8) s", ... (${wrong.length} total)" else ""))
          wrong.take(4).foreach { case (r, c) =>
            println(s"        (row $r, col $c): got ${got(r)(c)}, want ${want(r, c)}")
          }
        }
      }
      ok
    }
  }

  def printOutputs(label: String, outputs: Array[Array[BigInt]], numOutput: Int): Unit = {
    println(s"      $label (non-zero cycles, $numOutput ports):")
    var any = false
    for (t <- outputs.indices) {
      val row = outputs(t)
      if (row.exists(_ != 0)) {
        any = true
        println(f"        cycle $t%4d: ${row.mkString(", ")}")
      }
    }
    if (!any) println("        (none)")
  }

  // ════════════════════════════════════════════════════════════
  //  Overlapped drain capture
  // ════════════════════════════════════════════════════════════

  /**
   * A finished accumulator panel drains from its bank while the NEXT panel accumulates
   * into the other one (the [[AccSramDoubleBuffer]] ping-pong), so a panel's drain is
   * captured on the following panel's clock ticks instead of paying for a dedicated
   * bubble. This holds the panel awaiting validation and the samples gathered so far.
   */
  final class PendingDrain(
    val label:    String,
    val want:     (Int, Int) => BigInt,
    val depth:    Int,
    val capacity: Int,                       // in CYCLES, not values
  ) {
    /** One entry per captured CYCLE. It was a flat buffer of values while the compare
      * was a bag, which is exactly what discarded the port position. */
    val samples: scala.collection.mutable.ArrayBuffer[Array[BigInt]] =
      scala.collection.mutable.ArrayBuffer.empty[Array[BigInt]]
    def full: Boolean = samples.length >= capacity
  }

  /** The part of a poker the overlapped drain needs, so one loop serves both fabrics. */
  trait CapturePoker {
    /** One clock edge, no capture. */
    def tick(): Unit
    def readOutputs(): Array[BigInt]
    /** Park every driven input, including the accumulator control, at its idle value. */
    def zeroInputs(): Unit
  }

  /**
   * The part of a poker the stock accumulator PANEL schedule needs — the stationary load,
   * the streamed edge and the accumulator control — so one K-pass body serves Grouped and
   * Toroidal alike.
   *
   * `onTick` rather than `tick` is what the panel body clocks on: the multi-panel runners
   * hook the overlapped drain capture into it, and a blocking phase leaves it at its
   * default. That is the same reason it exists on the pokers at all.
   */
  trait PanelPoker extends CapturePoker {
    def loadStationaryTile(tile: Matrix): Unit
    def stock(i: Int, v: Int): Unit
    def stockDim: Int
    def streamValue(tile: Matrix, port: Int, t: Int): Int
    def acc(psumValid: Boolean, firstLoop: Boolean, lastPsum: Boolean): Unit
    def zeroStock(): Unit
    def onTick: () => Unit
  }

  /**
   * The overlapped drain-capture loop every accumulator runner shares.
   *
   * A panel is REGISTERED when its last psum burst has been driven, then captured on
   * the following panel's ticks. [[drainToFull]] tops up whatever the next panel's
   * driving window did not cover, with psumValid-low ticks — it must be called while
   * `drainSel` still points at the PREVIOUS panel's bank (i.e. just before this panel's
   * FINAL burst flips it), so it can never capture the wrong bank.
   */
  final class OverlappedDrain(poke: CapturePoker, capacity: Int, numPorts: Int) {
    private var pending: PendingDrain = null

    /** Register a finished panel as the pending drain. `want(slot, port)` is what its
      * accumulator slots should hold — see [[expectedPanelWindow]]. */
    def register(label: String, want: (Int, Int) => BigInt, depth: Int): Unit =
      pending = new PendingDrain(label, want, depth, capacity)

    /** One clock tick, capturing into the pending drain while it still has room. */
    def tick(): Unit = {
      poke.tick()
      if (pending != null && !pending.full) pending.samples += poke.readOutputs()
    }

    /** Compare and clear the pending panel. True (nothing to fail) when there is none. */
    def validate(): Boolean =
      if (pending == null) true
      else {
        val ok = compareExactDrain(
          pending.label, pending.want, pending.depth, numPorts, pending.samples.toArray).isDefined
        pending = null
        ok
      }

    /** Finish the pending capture with idle ticks, then validate. */
    def drainToFull(): Boolean = {
      while (pending != null && !pending.full) {
        poke.zeroInputs()
        tick()
      }
      validate()
    }
  }

  // ════════════════════════════════════════════════════════════
  //  Dual-mode array: geometry
  // ════════════════════════════════════════════════════════════

  /**
   * The ReuseA / ReuseB mirror of a Toroidal array — plain or dual-mode-wrapped — in one
   * place.
   *
   * Everything is written in terms of two abstract axes: the CONTRACTION axis the
   * stationary operand is chained along, and the PORT axis the outputs come out on.
   *
   *                     ReuseB                          ReuseA
   *   contraction       rows   (g = r*lvR + a)          columns (g = c*lvC + b)
   *   ports             cols   (q = c*lvC + b)          rows    (q = r*lvR + a)
   *   chain length L    totalVectorPeRow                totalVectorPeCol
   *
   * The same geometry serves BOTH schedules the array runs — the stock tiled GEMM and
   * the low-M GEMV — which is why the two settle constants live here side by side
   * rather than one of them being "the" settle. They are not interchangeable.
   *
   * It also serves the UNWRAPPED array: every width below is a formula over the config's
   * dimensions, and `toToroidalConfig` copies those dimensions verbatim, so a plain
   * `ToroidalConfig` and the dual config built around it answer identically. That is what
   * lets one runner body drive both.
   */
  final class ToroidalGeometry(val cfg: SystolicArrayConfig) {
    /** Whether the second-stream lanes and the mode selects are elaborated at all. */
    val isDual   = cfg.isDualMode
    val reuseB   = cfg.isReuseB
    val outerRow = cfg.outerPeRow
    val outerCol = cfg.outerPeCol
    val lvR      = cfg.localVectorPeRow
    val lvC      = cfg.localVectorPeCol
    val mult     = cfg.multPerPe

    /** Stationary-chain length = number of contraction slots. */
    val chainLen = if (reuseB) cfg.totalVectorPeRow else cfg.totalVectorPeCol
    /** Output ports = results produced per cycle. */
    val numPorts = if (reuseB) cfg.totalVectorPeCol else cfg.totalVectorPeRow
    /** K one pass through the fabric contracts (== contractionPassDepth). */
    val kTile    = chainLen * mult

    // Top-level port widths. In the GEMV schedule the stock systolic edge stays at zero
    // for the whole run; in the stock-GEMM schedule the dual lanes do.
    val stockDim      = (if (reuseB) outerCol else outerRow) * mult
    val stationaryDim = if (reuseB) cfg.systolicArrayColDimension else cfg.systolicArrayRowDimension
    val perOuterPe    = lvR * lvC * mult
    val dualDim       = cfg.totalVectorPeRow * cfg.totalVectorPeCol * mult
    val numUpdate     = numPorts // numUpdateEnableB == totalVectorPeCol, numUpdateEnableA == totalVectorPeRow

    /** Cycles from driving a GEMV stream step to that result reaching the array's output
      * (equivalently the accumulator's input). The leading `1` is the second stream's
      * registered entry stage at the DualMode boundary (`dualStreamBuffer`), which
      * mirrors the stock operand's own input buffer. */
    val latency = 1 + (if (reuseB) outerRow else outerCol) + 1 + log2Up(mult) + log2Up(if (reuseB) lvR else lvC)

    /**
     * Cycles to wait after the last weight-shift step before the first stream step, per
     * schedule. The chain latches slot `j` at cycle `j + chainLen - 1` (data advances
     * two registers per VectorPE hop, its capture enable only one — see
     * [[ToroidalPoker.loadStationaryBroadcast]]), so the deepest slot is not sealed until
     * `2*chainLen-2`.
     *
     *  - The STOCK schedule earns an outer-hop credit: the streamed operand's own
     *    one-hop-per-outer-PE propagation covers those hops, so `chainLen - outerPe`
     *    suffices — the stock dataflows' usual credit.
     *  - The GEMV schedule lost that credit in the adder-tree reorg: every lane is
     *    consumed on the FIRST stream cycle, so the full chain must be sealed first.
     *    This bound is TIGHT, not conservative — shaving one cycle streams against a
     *    still-latching deepest slot and corrupts the first stream step.
     *
     * A shared geometry must expose both and select on the schedule, never collapse
     * them.
     */
    val settleGemv = chainLen - 1
    val settleGemm = chainLen - (if (reuseB) outerRow else outerCol)

    def settleFor(schedule: Schedule.Value): Int =
      if (schedule == Schedule.Gemv) settleGemv else settleGemm

    /** Generous drain window for a no-accumulator tile run: worst-case last-output
      * latency through the C-chain. A window longer than the data is harmless — the exact
      * compare requires the extra cycles to be zero. */
    val drainTail = 3 * (cfg.totalVectorPeRow + cfg.totalVectorPeCol) + 2 +
      log2Up(mult) + log2Up(math.max(lvR, lvC))

    /** Contraction slot of a PE — its position along the stationary chain. */
    def slotOf(r: Int, c: Int, a: Int, b: Int): Int = if (reuseB) r * lvR + a else c * lvC + b

    /** Output port a PE feeds. */
    def portOf(r: Int, c: Int, a: Int, b: Int): Int = if (reuseB) c * lvC + b else r * lvR + a

    /** Second-stream index: VectorPE-major over outer PEs, then inner (a,b), then lane
      * — see ToroidalSystolicArray.wireDualModeA/B + ClusterPE.wireDualModeA/B. */
    def dualIndex(r: Int, c: Int, a: Int, b: Int, p: Int): Int =
      (r * outerCol + c) * perOuterPe + (a * lvC + b) * mult + p

    /**
     * Every (second-stream index -> contraction slot, output port) triple.
     *
     * This IS the top-level bus ordering `ToroidalSystolicArray.wireDualModeA/B` wires.
     * It lives here, once, precisely so that an RTL change to that ordering cannot be
     * made to pass by editing one of two copies.
     */
    val lanes: Seq[(Int, Int, Int)] = for {
      r <- 0 until outerRow
      c <- 0 until outerCol
      a <- 0 until lvR
      b <- 0 until lvC
      p <- 0 until mult
    } yield (dualIndex(r, c, a, b, p), slotOf(r, c, a, b) * mult + p, portOf(r, c, a, b))
  }

  // ════════════════════════════════════════════════════════════
  //  Dual-mode array: stimulus
  // ════════════════════════════════════════════════════════════

  /**
   * Stimulus helpers over a Toroidal array's mirrored port pairs — the unused side of
   * each pair is not elaborated, so every access has to pick a side.
   *
   * One poker covers BOTH schedules AND both builds:
   *
   *  - the mode is two register-deep select bits ([[setMode]]) and nothing else. Hold
   *    them low and the fabric is the plain Toroidal array the wrapper is built around;
   *    hold them high and it is the GEMV broadcast machine. That is what lets a
   *    mode-switch run be three phase calls on one DUT rather than a third
   *    reimplementation.
   *  - on an UNWRAPPED array the selects and the second-stream lanes are not elaborated
   *    at all (`generate`d away, so the fields are null). Every dual-only accessor is
   *    guarded by `g.isDual`, and the stock-GEMM half of the poker is exactly the
   *    sequence the unwrapped array's own driver used to run.
   */
  final class ToroidalPoker(dut: ToroidalSystolicArray[SInt, SInt], val g: ToroidalGeometry) extends PanelPoker {

    def stockDim: Int = g.stockDim

    /** Stock systolic edge — carries the moving operand in the stock GEMM schedule. */
    def stock(i: Int, v: Int): Unit = if (g.reuseB) dut.io.inputA(i) #= v else dut.io.inputB(i) #= v
    /** Per-VectorPE second stream — carries B in the GEMV schedule. Dual-mode only. */
    def dual(i: Int, v: Int): Unit  = if (g.reuseB) dut.io.inputA2(i) #= v else dut.io.inputB2(i) #= v

    /** Stationary bus: the stock port of whichever operand this dataflow holds, with
      * the broadcast demux in front of it inside the array. */
    def stationary(i: Int, v: Int): Unit = if (g.reuseB) dut.io.inputB(i) #= v else dut.io.inputA(i) #= v

    def update(v: Boolean): Unit =
      if (g.reuseB) for (i <- 0 until g.numUpdate) dut.io.inputUpdateEnableB(i) #= v
      else          for (i <- 0 until g.numUpdate) dut.io.inputUpdateEnableA(i) #= v

    def acc(psumValid: Boolean, firstLoop: Boolean, lastPsum: Boolean): Unit =
      if (g.cfg.needAccumulator)
        for (i <- 0 until dut.io.accControl.length) {
          dut.io.accControl(i).psumValid #= psumValid
          dut.io.accControl(i).firstLoop #= firstLoop
          dut.io.accControl(i).lastPsum  #= lastPsum
        }

    def zeroStock(): Unit      = for (i <- 0 until g.stockDim) stock(i, 0)
    def zeroDual(): Unit       = if (g.isDual) for (i <- 0 until g.dualDim) dual(i, 0)
    def zeroStationary(): Unit = for (i <- 0 until g.stationaryDim) stationary(i, 0)

    def zeroInputs(): Unit = {
      zeroStock()
      zeroDual()
      zeroStationary()
      update(false)
      acc(psumValid = false, firstLoop = false, lastPsum = false)
    }

    /**
     * Point the two mode selects at a schedule. Both cross one register inside the
     * array, so a run that never changes schedule sets this once before the reset and
     * never touches it again, and a mode-switch run flips it live between phases.
     */
    def setMode(schedule: Schedule.Value): Unit = {
      val dualMode = schedule == Schedule.Gemv
      require(g.isDual || !dualMode,
        s"[error] $schedule needs the dual-mode ports, which this array does not elaborate")
      if (g.isDual) {
        if (g.reuseB) dut.io.selectA #= dualMode else dut.io.selectB #= dualMode
        dut.io.stationarySelect #= dualMode
        // The muxed-out lanes are parked at zero so nothing floats.
        if (!dualMode) zeroDual()
      }
    }

    def reset(): Unit = {
      zeroInputs()
      dut.clockDomain.assertReset()
      dut.clockDomain.waitRisingEdge(2)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitRisingEdge(2)
    }

    def tick(): Unit = dut.clockDomain.waitRisingEdge()

    def readOutputs(): Array[BigInt] =
      Array.tabulate(g.numPorts)(i => dut.io.outputC(i).toBigInt)

    /**
     * GEMV: broadcast-load one K-slice of A down the weight chain, then wait for the
     * chain to finish latching.
     *
     * The chain shifts one contraction slot per load cycle while the capture enable
     * shifts with it, so the LAST value fed lands in slot 0 and the first in slot
     * chainLen-1: feeding `chainLen` steps puts step `s` in slot `chainLen-1-s`.
     * (Same "reversed" convention as the stock `flipRow` / `flipCol` shaping in
     * [[GoldenMatrix]].) `valueAt(slot, lane)` supplies the A element for a slot.
     */
    def loadStationaryBroadcast(valueAt: (Int, Int) => Int): Unit = {
      for (step <- 0 until g.chainLen) {
        for (p <- 0 until g.mult) stationary(p, valueAt(g.chainLen - 1 - step, p))
        update(true)
        onTick()
      }
      update(false)
      zeroStationary()
      // The far end's capture enable trails the near end's by one cycle per slot;
      // wait for the full chain to seal (the tree consumes every lane immediately).
      for (_ <- 0 until g.settleGemv) onTick()
    }

    /**
     * Stock GEMM: load one shaped stationary tile down the weight chain, one shift per
     * cycle. ReuseB feeds the tile's ROWS (the chain runs down the array rows), ReuseA
     * feeds the transposed tile's COLUMNS (the chain runs across the columns) — the same
     * shaping the stock Toroidal driver uses.
     *
     * The settle is NOT included: the accumulator path counts those cycles into its
     * overlapped drain capture, so it belongs to the caller.
     */
    def loadStationaryTile(tile: Matrix): Unit = {
      val steps = if (g.reuseB) tile.rows else tile.cols
      for (step <- 0 until steps) {
        if (g.reuseB) {
          val r = tile.row(step)
          for (p <- 0 until g.stationaryDim) stationary(p, if (p < r.length) r(p) else 0)
        } else {
          for (p <- 0 until g.stationaryDim) stationary(p, if (p < tile.rows) tile(p, step) else 0)
        }
        update(true)
        acc(psumValid = false, firstLoop = false, lastPsum = false)
        onTick()
      }
      update(false)
      zeroStationary()
    }

    /** One element of the stock-schedule streamed tile at (port, stream step). Toroidal
      * streams the tile's rows for ReuseB and the tile's columns (row index = port) for
      * ReuseA. */
    def streamValue(tile: Matrix, port: Int, t: Int): Int =
      if (g.reuseB) { val r = if (t < tile.rows) tile.row(t) else Array.empty[Int]; if (port < r.length) r(port) else 0 }
      else          { if (t < tile.cols && port < tile.rows) tile(port, t) else 0 }

    /** Stream steps one shaped tile takes in the stock schedule. */
    def streamCyclesOf(tile: GoldenMatrix.ShapedTile): Int =
      if (g.reuseB) tile.a.rows else tile.b.cols

    /** Overridable clock tick — the accumulator path hooks drain capture into it. */
    var onTick: () => Unit = () => dut.clockDomain.waitRisingEdge()
  }

  // ════════════════════════════════════════════════════════════
  //  Phase bodies
  // ════════════════════════════════════════════════════════════

  //  The accumulator bodies below return the DRAINED CAPTURE rather than validating it,
  //  so the same body serves a single-panel mode-switch phase and the multi-panel
  //  accumulator runners — the move that made `phaseGemm` / `phaseGemv` serve three
  //  schedules.

  /**
   * ONE K-pass of the stock accumulator panel schedule: load the stationary tile, wait out
   * the weight-load settle, then stream every slot with `psumValid` high for exactly
   * `numSlots * slotStride == depth` cycles.
   *
   * The burst length is FIXED at `depth`. The AccSram's address counter free-runs and
   * wraps there, so an unused slot streams ZEROS rather than shortening the burst — a
   * short burst would permanently shift the slot-to-output mapping and every later pass
   * would accumulate on the wrong addresses.
   *
   * `beforeBurst` runs after the settle and before the first psum cycle. That is the one
   * safe point for the overlapped-drain top-up: `drainSel` still points at the PREVIOUS
   * panel's bank, because this panel's `burstEnded` has not fired yet.
   */
  def accGemmKPass(
    poke:        PanelPoker,
    stationary:  Matrix,
    settle:      Int,
    slotAt:      Int => Option[Matrix],
    numSlots:    Int,
    slotStride:  Int,
    firstK:      Boolean,
    lastK:       Boolean,
    beforeBurst: () => Unit = () => (),
  ): Unit = {
    poke.loadStationaryTile(stationary)
    for (_ <- 0 until settle) poke.onTick()

    beforeBurst()

    for (slot <- 0 until numSlots) {
      val streamed = slotAt(slot)
      for (t <- 0 until slotStride) {
        for (p <- 0 until poke.stockDim)
          poke.stock(p, streamed.map(m => poke.streamValue(m, p, t)).getOrElse(0))
        poke.acc(
          psumValid = true,
          firstLoop = firstK,                                     // overwrite on the first K-pass
          lastPsum  = lastK && slot == numSlots - 1 && t == slotStride - 1,
        )
        poke.onTick()
      }
    }
    poke.zeroStock()
    poke.acc(psumValid = false, firstLoop = false, lastPsum = false)
  }

  /**
   * ONE K-pass of the GEMV accumulator schedule: broadcast-load the A slice, then stream
   * `depth` steps on the per-VectorPE lanes with `psumValid` high.
   *
   * Control and data are driven TOGETHER, exactly as in the stock dataflows: the second
   * stream crosses its own registered entry stage at the DualMode boundary, which makes
   * the dual path as deep as the stock one the accControl delay chain is tuned for.
   *
   * `stationaryAt(pass, slot, lane)` and `streamAt(pass, k, n)` take the pass index
   * because the K-pass is what selects the A slice and the B rows; `n` is the panel's
   * ABSOLUTE output index.
   */
  def accGemvKPass(
    poke:         ToroidalPoker,
    stationaryAt: (Int, Int, Int) => Int,
    streamAt:     (Int, Int, Int) => Int,
    base:         Int,
    depth:        Int,
    pass:         Int,
    firstPass:    Boolean,
    lastPass:     Boolean,
  ): Unit = {
    val g = poke.g
    poke.loadStationaryBroadcast((slot, p) => stationaryAt(pass, slot, p))
    for (c <- 0 until depth) {
      poke.acc(psumValid = true, firstLoop = firstPass, lastPsum = lastPass && c == depth - 1)
      for ((idx, k, q) <- g.lanes) poke.dual(idx, streamAt(pass, k, base + c * g.numPorts + q))
      poke.onTick()
    }
    poke.zeroDual()
    poke.acc(psumValid = false, firstLoop = false, lastPsum = false)
  }

  /** Capture `cycles` clock edges of a BLOCKING drain, without touching the inputs — the
    * K-pass body already parked them. */
  def captureDrain(poke: CapturePoker, cycles: Int, numPorts: Int): Array[Array[BigInt]] = {
    val captured = Array.fill(cycles, numPorts)(BigInt(0))
    for (i <- 0 until cycles) {
      poke.tick()
      captured(i) = poke.readOutputs()
    }
    captured
  }

  /**
   * One WHOLE stock-GEMM accumulator panel on an already-reset, already-Gemm-mode DUT:
   * every K-pass of it, then a blocking drain, handed back uncchecked.
   *
   * Blocking because a mode-switch phase has nothing to overlap with — `OverlappedDrain`
   * is for a panel SEQUENCE. The multi-panel runners share [[accGemmKPass]] with this and
   * keep their overlapped capture.
   */
  def phaseGemmAccPanel(
    poke:        ToroidalPoker,
    panel:       Seq[GoldenMatrix.ShapedTile],
    depth:       Int,
    slotStride:  Int,
    drainWindow: Int,
  ): Array[Array[BigInt]] = {
    val g        = poke.g
    val numSlots = depth / slotStride
    def swept(t: GoldenMatrix.ShapedTile): Int = if (g.reuseB) t.index.m else t.index.n
    val ks     = panel.map(_.index.k).distinct.sorted
    val slots  = panel.map(swept).distinct.sorted
    val bySlot = panel.map(t => (t.index.k, swept(t)) -> t).toMap

    for ((k, ki) <- ks.zipWithIndex) {
      val shared = bySlot((k, slots.head))
      accGemmKPass(
        poke,
        stationary = if (g.reuseB) shared.b else shared.a,
        settle     = g.settleGemm,
        slotAt     = slot => slots.lift(slot).map(sv => if (g.reuseB) bySlot((k, sv)).a else bySlot((k, sv)).b),
        numSlots   = numSlots,
        slotStride = slotStride,
        firstK     = ki == 0,
        lastK      = ki == ks.size - 1,
      )
    }
    captureDrain(poke, drainWindow, g.numPorts)
  }

  /** One WHOLE GEMV accumulator panel: every K-pass, then a blocking drain, handed back
    * unchecked. The GEMV counterpart of [[phaseGemmAccPanel]]. */
  def phaseGemvAccPanel(
    poke:         ToroidalPoker,
    stationaryAt: (Int, Int, Int) => Int,
    streamAt:     (Int, Int, Int) => Int,
    base:         Int,
    depth:        Int,
    numKPasses:   Int,
    drainWindow:  Int,
  ): Array[Array[BigInt]] = {
    for (pass <- 0 until numKPasses)
      accGemvKPass(poke, stationaryAt, streamAt, base, depth, pass,
        firstPass = pass == 0, lastPass = pass == numKPasses - 1)
    captureDrain(poke, drainWindow, poke.g.numPorts)
  }

  //  A phase body owns NEITHER `SimConfig.compile` NOR the reset. Both belong to the
  //  caller, so a phase can run on an already-reset DUT and hand back its capture.
  //
  //  That one rule is what makes a mode-switch run three calls instead of a third
  //  reimplementation of the two schedules, and it is what lets reset POLICY — one reset
  //  per tile for the no-accumulator path, exactly one for the accumulator's ping-pong —
  //  be the caller's decision rather than a fact baked into each body.

  /**
   * One stock-GEMM tile on an already-reset, already-Gemm-mode DUT: load the stationary
   * operand, wait out the weight-load settle, stream the moving operand, and capture
   * every output cycle through the drain tail.
   */
  def phaseGemm(
    poke:      ToroidalPoker,
    tile:      GoldenMatrix.ShapedTile,
    drainTail: Int,
  ): Array[Array[BigInt]] = {
    val g = poke.g

    //1. Pre-load the stationary operand (B for ReuseB, A for ReuseA).
    poke.loadStationaryTile(if (g.reuseB) tile.b else tile.a)
    for (_ <- 0 until g.settleGemm) poke.onTick()

    //2. Stream the moving operand and capture every output cycle.
    val streamed      = if (g.reuseB) tile.a else tile.b
    val streamCycles  = poke.streamCyclesOf(tile)
    val captureCycles = streamCycles + drainTail
    val captured      = Array.fill(captureCycles, g.numPorts)(BigInt(0))
    for (t <- 0 until captureCycles) {
      if (t < streamCycles) for (p <- 0 until g.stockDim) poke.stock(p, poke.streamValue(streamed, p, t))
      else                  poke.zeroStock()
      poke.tick()
      captured(t) = poke.readOutputs()
    }
    captured
  }

  /**
   * One low-M GEMV burst on an already-reset, already-Gemv-mode DUT: broadcast-load the
   * A slice down the stationary chain, stream B on the per-VectorPE lanes for `steps`
   * cycles, and capture through the drain tail.
   *
   * `stationaryAt(slot, lane)` supplies the A element of a contraction slot;
   * `streamAt(k, n)` supplies B for contraction index `k` and output index `n`.
   */
  def phaseGemv(
    poke:         ToroidalPoker,
    stationaryAt: (Int, Int) => Int,
    streamAt:     (Int, Int) => Int,
    steps:        Int,
    drainTail:    Int,
  ): Array[Array[BigInt]] = {
    val g = poke.g

    poke.loadStationaryBroadcast(stationaryAt)

    val captureCycles = steps + drainTail
    val captured      = Array.fill(captureCycles, g.numPorts)(BigInt(0))
    for (t <- 0 until captureCycles) {
      if (t < steps) for ((idx, k, q) <- g.lanes) poke.dual(idx, streamAt(k, t * g.numPorts + q))
      else           poke.zeroDual()
      poke.tick()
      captured(t) = poke.readOutputs()
    }
    captured
  }

  // ════════════════════════════════════════════════════════════
  //  Grouped array: geometry
  // ════════════════════════════════════════════════════════════

  /**
   * The ReuseA / ReuseB mirror of a Grouped array, in one place — [[ToroidalGeometry]]'s
   * counterpart for the other fabric.
   *
   * Which operand is stationary is a PARAMETER, not a routing decision: the two Grouped
   * accumulator runners this replaces differed only by the substitution A<->B, m<->n,
   * row<->col, down to their local helpers, and said one thing twice in 336 lines.
   *
   *                     ReuseA                      ReuseB
   *   stationary        A(m,k), by its columns      B(k,n), by its rows
   *   streamed          B, transposed tile rows     A, transposed tile columns
   *   panel holds fixed m                           n
   *   panel sweeps      n                           m
   *   weight chain      columns (totalVectorPeCol)  rows (totalVectorPeRow)
   *
   * The two peculiarities that separate Grouped from Toroidal — streaming the TRANSPOSED
   * tile and reserving a de-skew zero-flush slot — are SHARED between its two dataflows,
   * so they are no obstacle to this merge even though they block the fabric one.
   */
  final class GroupedGeometry(val cfg: SystolicArrayConfig) {
    require(cfg.isGrouped,  s"[error] GroupedGeometry expects a Grouped config, got ${cfg.fabric}")
    require(cfg.isReuseAB,  s"[error] GroupedGeometry expects ReuseA/ReuseB, got ${cfg.dataflow}")

    private val topology = cfg.topology

    val reuseA   = cfg.isReuseA
    /** Output ports = results produced per cycle. */
    val numPorts = topology.systolicOutputDimC
    /** Weight-load enables of whichever operand this dataflow holds stationary. */
    val numUpdate = if (reuseA) topology.numUpdateEnableA else topology.numUpdateEnableB

    /**
     * Cycles to wait after the last weight-shift step before the first stream step:
     * the shift-chain depth (columns for ReuseA, rows for ReuseB) minus the outer-PE
     * hops the streamed operand's own propagation overlaps.
     */
    val settleGemm =
      if (reuseA) cfg.totalVectorPeCol - cfg.outerPeCol
      else        cfg.totalVectorPeRow - cfg.outerPeRow

    /** Cycles the de-skew + C-chain takes to flush after the last psum cycle. */
    val pipeTail = cfg.outerPeRow + cfg.outerPeCol + log2Up(cfg.multPerPe) +
      log2Up(math.max(cfg.localVectorPeRow, cfg.localVectorPeCol)) + 2

    /** Generous drain window for a no-accumulator tile run: worst-case last-output
      * latency through the de-skew + C-chain. */
    val drainTail = 3 * (cfg.totalVectorPeRow + cfg.totalVectorPeCol) + 2 +
      log2Up(cfg.multPerPe) + log2Up(math.max(cfg.localVectorPeRow, cfg.localVectorPeCol))

    /** Accumulator slots one swept tile occupies — the swept dimension's tile extent. */
    def slotStrideOf(shape: GoldenMatrix.TileShape): Int = if (reuseA) shape.tileN else shape.tileM

    /** (index the panel HOLDS FIXED, which block of the swept dimension it covers). */
    def panelKey(t: GoldenMatrix.ShapedTile, tilesPerPanel: Int): (Int, Int) =
      if (reuseA) (t.index.m, t.index.n / tilesPerPanel) else (t.index.n, t.index.m / tilesPerPanel)

    /** Index of the dimension a panel sweeps — N for ReuseA, M for ReuseB. */
    def sweptIndex(t: GoldenMatrix.ShapedTile): Int = if (reuseA) t.index.n else t.index.m

    /** The (m, n) an output block sits at, given the panel's fixed index and a slot. */
    def blockAt(fixed: Int, swept: Int): (Int, Int) = if (reuseA) (fixed, swept) else (swept, fixed)
  }

  // ════════════════════════════════════════════════════════════
  //  Stock (Grouped / Toroidal) arrays: stimulus
  // ════════════════════════════════════════════════════════════

  /**
   * Stimulus helpers over a plain Grouped or Toroidal array.
   *
   * Grouped and Toroidal are different Scala classes, which is what blocked sharing
   * before; `Component with SystolicArray[SInt, SInt]` is the bound `runGroupedReuseABNoAcc`
   * already used to drive both from one body.
   *
   * The optional ports (`inputUpdateEnableA/B`, `inputAccEnableC`, `accControl`) are
   * `generate`d, i.e. null when the config does not ask for them, so every accessor
   * here is guarded by the same predicate that elaborates the port.
   */
  final class StockPoker(dut: Component with SystolicArray[SInt, SInt], val cfg: SystolicArrayConfig)
    extends PanelPoker {
    private val topology = cfg.topology

    val numInputA  = topology.systolicInputDimA
    val numInputB  = topology.systolicInputDimB
    val numOutput  = topology.systolicOutputDimC

    def inputA(i: Int, v: Int): Unit = dut.io.inputA(i) #= v
    def inputB(i: Int, v: Int): Unit = dut.io.inputB(i) #= v

    /** Weight-load enable of whichever operand this dataflow holds stationary. */
    def update(v: Boolean): Unit =
      if (cfg.isReuseA)      for (i <- 0 until topology.numUpdateEnableA) dut.io.inputUpdateEnableA(i) #= v
      else if (cfg.isReuseB) for (i <- 0 until topology.numUpdateEnableB) dut.io.inputUpdateEnableB(i) #= v

    /** ReuseC's in-PE accumulate enable — the output-stationary dataflow's only control. */
    def accEnableC(v: Boolean): Unit =
      if (cfg.isReuseC) for (i <- 0 until topology.numAccEnableC) dut.io.inputAccEnableC(i) #= v

    def acc(psumValid: Boolean, firstLoop: Boolean, lastPsum: Boolean): Unit =
      if (cfg.needAccumulator)
        for (i <- 0 until topology.numAccControl) {
          dut.io.accControl(i).psumValid #= psumValid
          dut.io.accControl(i).firstLoop #= firstLoop
          dut.io.accControl(i).lastPsum  #= lastPsum
        }

    /**
     * Stationary bus: the port of whichever operand this dataflow holds stationary
     * (A for ReuseA, B for ReuseB), and the stock systolic edge the other one streams
     * on. The same two members [[ToroidalPoker]] has, so one merged body drives the
     * mirror pair on this fabric too.
     */
    def stationary(i: Int, v: Int): Unit = if (cfg.isReuseA) inputA(i, v) else inputB(i, v)
    def stock(i: Int, v: Int): Unit      = if (cfg.isReuseA) inputB(i, v) else inputA(i, v)

    val stationaryDim = if (cfg.isReuseA) numInputA else numInputB
    val stockDim      = if (cfg.isReuseA) numInputB else numInputA

    def zeroStationary(): Unit = for (i <- 0 until stationaryDim) stationary(i, 0)
    def zeroStock(): Unit      = for (i <- 0 until stockDim) stock(i, 0)

    /**
     * Load one shaped stationary tile down the weight chain, one shift per cycle.
     * ReuseA feeds the tile's COLUMNS (the chain runs across the array columns), ReuseB
     * feeds its ROWS (the chain runs down the rows).
     *
     * The settle is NOT included: the accumulator path counts those cycles into its
     * overlapped drain capture, so it belongs to the caller.
     */
    def loadStationaryTile(tile: Matrix): Unit = {
      val steps = if (cfg.isReuseA) tile.cols else tile.rows
      for (step <- 0 until steps) {
        if (cfg.isReuseA)
          for (p <- 0 until stationaryDim) stationary(p, if (p < tile.rows) tile(p, step) else 0)
        else {
          val r = tile.row(step)
          for (p <- 0 until stationaryDim) stationary(p, if (p < r.length) r(p) else 0)
        }
        update(true)
        acc(psumValid = false, firstLoop = false, lastPsum = false)
        onTick()
      }
      update(false)
      zeroStationary()
    }

    /**
     * One element of the streamed tile at (port, stream step). Grouped streams the
     * TRANSPOSED tile, so ReuseA reads the streamed B's rows and ReuseB the streamed A's
     * columns — the reflection of [[ToroidalPoker.streamValue]], whose tiles are not
     * transposed.
     */
    def streamValue(tile: Matrix, port: Int, t: Int): Int =
      if (cfg.isReuseA) { val r = if (t < tile.rows) tile.row(t) else Array.empty[Int]
                          if (port < r.length) r(port) else 0 }
      else              { if (t < tile.cols && port < tile.rows) tile(port, t) else 0 }

    /** Overridable clock tick — the accumulator path hooks drain capture into it. */
    var onTick: () => Unit = () => dut.clockDomain.waitRisingEdge()

    def zeroA(): Unit = for (i <- 0 until numInputA) inputA(i, 0)
    def zeroB(): Unit = for (i <- 0 until numInputB) inputB(i, 0)

    def zeroInputs(): Unit = {
      zeroA()
      zeroB()
      update(false)
      accEnableC(false)
      acc(psumValid = false, firstLoop = false, lastPsum = false)
    }

    def reset(): Unit = {
      zeroInputs()
      dut.clockDomain.assertReset()
      dut.clockDomain.waitRisingEdge(2)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitRisingEdge(2)
    }

    def tick(): Unit = dut.clockDomain.waitRisingEdge()

    def readOutputs(): Array[BigInt] =
      Array.tabulate(numOutput)(i => dut.io.outputC(i).toBigInt)
  }
}
