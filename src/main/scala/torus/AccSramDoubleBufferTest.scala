package torus

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

import torus.accumulator._
import torus.arithmetic._
import torus.config._
import torus.core._

/**
 * Focused component-level test for [[AccSramDoubleBuffer]] ping-pong / overlap.
 *
 * It exists because the end-to-end tests structurally CANNOT reach the second bank: they
 * call resetArray() between output panels, so `writeSel == 1` is never selected no matter
 * how many GEMMs they run. This drives the block directly with consecutive panels and NO
 * reset between them, which is the only way that half of the hardware is exercised at
 * all, and checks both drain correctly across a range of inter-panel gaps.
 *
 * This test mirrors the ACTUAL [[DataflowDriver]] acc protocol, which is the
 * only pattern [[AccSram]] is designed for:
 *  - depth = `freeDimensionSize` (the output-indexed
 *    AccSram depth).
 *  - Each K-pass (loop) streams the full `numSlots` slots contiguously with
 *    `psumValid` high; `firstLoop` is high for the WHOLE first K-pass (overwrite),
 *    `lastPsum` only on the very last psum of the last K-pass (arms the drain).
 *    The burst length per K-pass must equal `depth` exactly — AccSram's address
 *    counter free-runs on `psumValid` and is never reset.
 *  - The LAST tile of every panel (`tilesPerPanel = numSlots - 1` carry data) is a
 *    reserved zero-flush / de-skew tile. This matters: AccSram's `mem.readSync`
 *    uses the default read-under-write policy, so the drain read of the slot(s)
 *    right at the address wrap collides with the same-cycle zero-write and reads
 *    garbage. The driver reserves that trailing tile as zeros precisely so the
 *    garbage never lands on a real value; this test does the same.
 *  - drainLen is `depth`, not `2 * depth` (SRAM read-and-zero pass wraps once).
 *  - The block I/O is a `numPorts`-wide Vec; scenarios A-C drive a single port
 *    (numPorts = 1) so the ping-pong behaviour is read off one stream.
 *
 * Scenario D is the multi-port randomized sweep that used to live in
 * [[TestMain]] as its `--block` stage: every one of the config's output ports is
 * driven with aligned random stimulus for a few (depth, loops) shapes and the
 * drained non-zero set per port is compared against a software golden. It is
 * here rather than in TestMain because it exercises the same block through the
 * same acc protocol, and TestMain's job is the end-to-end (config, GEMM) sweep.
 *
 *   sbt "runMain torus.AccSramDoubleBufferTest"
 *
 * ALL scenarios pass as of this writing; the whole file is a REGRESSION guard now. It was
 * written against the pre-fix RTL, where `gap = 1` failed because a single
 * `writeToBuffer1` flipped only at the END of a drain, so panel 2's psums collided with
 * panel 1's drain. The fix was to split that into independent `writeSel` / `drainSel`
 * banks plus bank re-alignment (see [[AccSramDoubleBuffer]]). Read a failure here as that
 * split having regressed, not as a test awaiting a fix.
 */
object AccSramDoubleBufferTest {

  private val numPorts = 1

  // Named as the drivers name them (DataflowDriver): `slotStride` addresses per
  // output tile, `numSlots` tiles per address wrap, `depth` = their product.
  private val slotStride = 4                            // addresses per output tile
  private val numSlots   = 3                            // output tiles per address wrap
  private val depth      = slotStride * numSlots        // = 12, AccSram depth (= freeDimensionSize)

  private val tilesPerPanel = numSlots - 1              // real tiles; last tile is zero-flush
  private val realSlots     = tilesPerPanel * slotStride // = 8 slots carry real data

  private val cfg = ToroidalConfig(
    numericType              = NumericType.Signed,
    dataflow                 = Dataflow.ReuseB,
    outerPeRow               = 2,
    outerPeCol               = 2,
    localVectorPeRow         = 1,
    localVectorPeCol         = 1,
    multPerPe                = 1,
    bitWidthA                = 8,
    bitWidthB                = 8,
    needAccumulator          = true,
    freeDimensionSize        = depth,
    // Each panel below is 2 K-passes; one pass consumes
    // `contractionPassDepth` (= systolicArrayRowDimension = 2) K values, so the
    // accumulator data width must be provisioned for K = 2 * 2.
    maxContractionSize       = 4,
  )

  // Each panel = numLoops K-passes; real value at slot s = sum over K-passes.
  // panel0: loop0 = 500+s, loop1 = 3   -> exp0 = 503+s   (s = 0..realSlots-1)
  // panel1: loop0 = 700+s, loop1 = 5   -> exp1 = 705+s
  private val panel0 = Seq(
    (0 until realSlots).map(s => 500 + s),
    (0 until realSlots).map(_ => 3),
  )
  private val exp0 = (0 until realSlots).map(s => 503 + s)

  private val panel1 = Seq(
    (0 until realSlots).map(s => 700 + s),
    (0 until realSlots).map(_ => 5),
  )
  private val exp1 = (0 until realSlots).map(s => 705 + s)

  // panel2: loop0 = 900+s, loop1 = 7 -> exp2 = 907+s. Reuses bank0 (panel0's
  // bank), so it exercises the SRAM addr-alignment risk: after panel0's drain
  // the bank's free-running addr ends at 1 (not 0) and stays frozen while panel1
  // runs on the other bank. If the fix does not re-align it, panel2's firstLoop
  // overwrite lands on the wrong slot. A 2-panel test cannot catch this.
  private val panel2 = Seq(
    (0 until realSlots).map(s => 900 + s),
    (0 until realSlots).map(_ => 7),
  )
  private val exp2 = (0 until realSlots).map(s => 907 + s)

  private val drainLen = depth // SRAM read-and-zero pass wraps the addr once

  /**
   * Scenario D: all output ports at once, random stimulus, software golden.
   *
   * One panel of `nLoops` K-passes over the full `depth`, every port driven with
   * its own random values on the same cycles (the drivers always present the
   * whole output-port Vec together). Bank depth is set from `depth` via
   * `freeDimensionSize` so the burst length matches the address wrap exactly.
   *
   * The check is set-based per port — the drain order is not what is under test
   * here, the per-slot sums are — so a zero sum is dropped from both sides.
   */
  private def runMultiPortCase(depth: Int, nLoops: Int, seed: Int): Boolean = {
    val caseCfg = ToroidalConfig(
      numericType              = NumericType.Signed,
      dataflow                 = Dataflow.ReuseB,
      outerPeRow               = 4,
      outerPeCol               = 4,
      localVectorPeRow         = 1,
      localVectorPeCol         = 1,
      multPerPe                = 1,
      bitWidthA                = 8,
      bitWidthB                = 8,
      needAccumulator          = true,
      // the bank depth IS the free output dimension, and it matches the per-slot
      // stimulus length below.
      freeDimensionSize        = depth,
      // The accumulator data width must cover `nLoops` summed partial sums; one
      // loop == one K-pass of `contractionPassDepth` (= systolicArrayRowDimension
      // = 4 here) K values, so provision K = nLoops * 4.
      maxContractionSize       = nLoops * 4,
    )

    implicit val arith: Arithmetic[SInt, SInt]       = Arithmetic.sIntArithmetic
    implicit val pt:    PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(caseCfg)

    val ports = caseCfg.topology.systolicOutputDimC

    val rng = new scala.util.Random(seed)
    // stimulus(loop)(slot)(port)
    val stimulus = Seq.fill(nLoops)(Array.fill(depth, ports)(rng.nextInt(101) - 50))

    // golden(port) = set of non-zero per-slot sums across loops
    val goldenPerPort: Seq[Set[BigInt]] = (0 until ports).map { p =>
      (0 until depth).map(s => stimulus.map(_(s)(p)).sum).filter(_ != 0).map(BigInt(_)).toSet
    }

    val drainedPerPort = Array.fill(ports)(ArrayBuffer[BigInt]())

    SimConfig.compile(new AccSramDoubleBuffer[SInt, SInt](caseCfg, ports)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      def zero(): Unit = {
        for (p <- 0 until ports) dut.io.input(p) #= 0
        dut.io.accControl.psumValid #= false; dut.io.accControl.firstLoop #= false; dut.io.accControl.lastPsum #= false
      }

      zero()
      dut.clockDomain.assertReset(); dut.clockDomain.waitRisingEdge(2)
      dut.clockDomain.deassertReset(); dut.clockDomain.waitRisingEdge(2)

      for ((loop, li) <- stimulus.zipWithIndex) {
        val first = li == 0
        val last  = li == nLoops - 1
        for (s <- 0 until depth) {
          for (p <- 0 until ports) dut.io.input(p) #= loop(s)(p)
          dut.io.accControl.psumValid #= true
          dut.io.accControl.firstLoop #= first
          dut.io.accControl.lastPsum  #= last && s == depth - 1
          dut.clockDomain.waitRisingEdge()
        }
      }

      zero()
      for (_ <- 0 until 2 * depth + 8) {
        dut.clockDomain.waitRisingEdge()
        for (p <- 0 until ports) drainedPerPort(p) += dut.io.output(p).toBigInt
      }
    }

    var pass = true
    for (p <- 0 until ports) {
      val got = drainedPerPort(p).iterator.filter(_ != 0).toSet
      val ok  = got == goldenPerPort(p)
      pass &= ok
      if (!ok) {
        println(s"    [fail] port $p: missing=${(goldenPerPort(p) -- got).toSeq.sorted}, " +
          s"extra=${(got -- goldenPerPort(p)).toSeq.sorted}")
      }
    }

    println(f"[D] multi-port random, depth=$depth%-2d loops=$nLoops%-2d ports=$ports%-2d seed=$seed:  " +
      f"${if (pass) "PASS" else "FAIL"}")
    pass
  }

  def main(args: Array[String]): Unit = {
    implicit val arith: Arithmetic[SInt, SInt]    = Arithmetic.sIntArithmetic
    implicit val pt: PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(cfg)

    var allPass = true

    SimConfig.compile(new AccSramDoubleBuffer[SInt, SInt](cfg, numPorts)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      def resetDut(): Unit = {
        dut.io.accControl.psumValid #= false
        dut.io.accControl.firstLoop #= false
        dut.io.accControl.lastPsum  #= false
        for (p <- 0 until numPorts) dut.io.input(p) #= 0
        dut.clockDomain.assertReset()
        dut.clockDomain.waitRisingEdge(2)
        dut.clockDomain.deassertReset()
        dut.clockDomain.waitRisingEdge(2)
      }

      // capture only port 0 (single-port instance)
      def cap0(cap: ArrayBuffer[BigInt]): Unit = cap += dut.io.output(0).toBigInt

      // Drive one panel: `loops` K-passes, each streaming the full `depth` cycles
      // (numSlots slots). Real data goes in the first `tilesPerPanel` tiles; the
      // trailing tile is a zero-flush. firstLoop spans the whole first K-pass;
      // lastPsum pulses on the final psum of the final K-pass.
      def drivePanel(loops: Seq[Seq[Int]], cap: ArrayBuffer[BigInt]): Unit = {
        val n = loops.length
        for (l <- 0 until n; slot <- 0 until numSlots; t <- 0 until slotStride) {
          val s      = slot * slotStride + t
          val isReal = slot < tilesPerPanel
          dut.io.accControl.psumValid #= true
          dut.io.accControl.firstLoop #= (l == 0)
          dut.io.accControl.lastPsum  #= (l == n - 1 && slot == numSlots - 1 && t == slotStride - 1)
          dut.io.input(0)  #= (if (isReal) loops(l)(s) else 0)
          dut.clockDomain.waitRisingEdge()
          cap0(cap)
        }
      }

      def idle(cycles: Int, cap: ArrayBuffer[BigInt]): Unit = {
        for (_ <- 0 until cycles) {
          dut.io.accControl.psumValid #= false
          dut.io.accControl.firstLoop #= false
          dut.io.accControl.lastPsum  #= false
          dut.io.input(0)  #= 0
          dut.clockDomain.waitRisingEdge()
          cap0(cap)
        }
      }

      // does `cap` contain `want` as a contiguous run?
      def containsRun(cap: ArrayBuffer[BigInt], want: Seq[Int]): Boolean = {
        val w = want.map(BigInt(_))
        (0 to cap.length - w.length).exists(s => (0 until w.length).forall(k => cap(s + k) == w(k)))
      }

      // ── Scenario A: single panel through the double buffer (sanity) ──
      {
        resetDut()
        val cap = ArrayBuffer.empty[BigInt]
        drivePanel(panel0, cap)
        idle(drainLen + 10, cap)
        val ok = containsRun(cap, exp0)
        println(f"[A] single panel:                ${if (ok) "PASS" else "FAIL"}  (expect ${exp0.mkString(",")})")
        if (!ok) println(s"    capture=${cap.mkString(",")}")
        allPass &= ok
      }

      // ── Scenario B: two panels back-to-back, varying inter-panel gap ──
      // gap = number of psumValid-low cycles between panel0's last psum and
      // panel1's first psum. gap>=1 is required to raise burstEnded (drain panel0).
      // The gaps are chosen to straddle the drain: 1 is the tightest overlap the
      // protocol allows (panel1 writes while panel0 still drains — the case the
      // single-select RTL got wrong), `depth`+ leaves the drain fully finished, and
      // `slotStride` sits between the two.
      for (gap <- Seq(1, 2, slotStride, depth, depth + 4).distinct) {
        resetDut()
        val cap = ArrayBuffer.empty[BigInt]
        drivePanel(panel0, cap)
        idle(gap, cap)
        drivePanel(panel1, cap)
        idle(drainLen + 10, cap)
        val ok0 = containsRun(cap, exp0)
        val ok1 = containsRun(cap, exp1)
        val ok  = ok0 && ok1
        println(f"[B] two panels, gap=$gap%-2d:           ${if (ok) "PASS" else "FAIL"}  " +
          f"(panel0 ${if (ok0) "ok" else "MISS"}, panel1 ${if (ok1) "ok" else "MISS"})")
        if (!ok) println(s"    capture=${cap.mkString(",")}")
        allPass &= ok
      }

      // ── Scenario C: THREE panels back-to-back (gap = 1) ──
      // The sharpest of the four: panel2 reuses panel0's bank, so it is the only
      // scenario that exercises bank RE-ALIGNMENT. After panel0 drains, that bank's
      // free-running address sits at 1 rather than 0 and stays frozen while panel1 runs
      // on the other bank; without re-alignment panel2's firstLoop overwrite lands on the
      // wrong slot. Two panels can never reach this state — which is why scenario B
      // passed on RTL that this one caught.
      for (gap <- Seq(1, depth + 4).distinct) {
        resetDut()
        val cap = ArrayBuffer.empty[BigInt]
        drivePanel(panel0, cap)
        idle(gap, cap)
        drivePanel(panel1, cap)
        idle(gap, cap)
        drivePanel(panel2, cap)
        idle(drainLen + 10, cap)
        val ok0 = containsRun(cap, exp0)
        val ok1 = containsRun(cap, exp1)
        val ok2 = containsRun(cap, exp2)
        val ok  = ok0 && ok1 && ok2
        println(f"[C] three panels, gap=$gap%-2d:         ${if (ok) "PASS" else "FAIL"}  " +
          f"(panel0 ${if (ok0) "ok" else "MISS"}, panel1 ${if (ok1) "ok" else "MISS"}, panel2 ${if (ok2) "ok" else "MISS"})")
        if (!ok) println(s"    capture=${cap.mkString(",")}")
        allPass &= ok
      }
    }

    // ── Scenario D: every output port at once, random stimulus (was TestMain's
    // `--block` stage). Each case elaborates its own DUT because the bank depth
    // comes from the config.
    allPass &= Seq(
      runMultiPortCase(depth = 4, nLoops = 3, seed = 1),
      runMultiPortCase(depth = 8, nLoops = 1, seed = 2),
      runMultiPortCase(depth = 8, nLoops = 4, seed = 3),
      runMultiPortCase(depth = 6, nLoops = 5, seed = 4),
    ).forall(identity)

    println(s"\n[AccSramDoubleBufferTest] ${if (allPass) "ALL PASS" else "FAILURES PRESENT"}")
    if (!allPass) sys.exit(1)
  }
}
