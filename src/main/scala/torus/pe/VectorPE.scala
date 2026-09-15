package torus.pe

import spinal.core._
import spinal.lib.Delay

import torus.accumulator._
import torus.arithmetic._
import torus.config._
import torus.core._

/**
 * The LEAF of the hierarchy: `multPerPe` multipliers, one adder tree, and whatever the
 * dataflow needs holding between cycles. Every product in the fabric is computed here.
 *
 * It sits at one of two depths, and `isStandAloneVectorPeConfig` is which:
 *
 *  - Inside a cluster, built by `OuterPE.createVectorPes` with a real [[InnerIndex]].
 *  - Directly on the array when `lvR == lvC == 1`, built by `SystolicArray.createVectorPe`
 *    with `InnerIndex(0, 0)` and no cluster level at all.
 *
 * Both cases are this one class, which is why the build methods keep branching on that
 * predicate: at the array level the PE owns the C-chain register and the array's skew
 * budget, while inside a cluster the enclosing [[OuterPE]] owns them and this PE must NOT
 * add a second one. Most of the apparent special-casing below is that single question.
 *
 * The constructor picks ONE of three bodies and nothing is shared between them:
 *
 *  - `buildReuseA` — A is captured into a register when `updateEnableA` pulses and held;
 *    B streams past. The captured value, not the input, is what is forwarded, so the
 *    stationary operand propagates as it loads.
 *  - `buildReuseB` — the exact mirror, B captured and A streaming. The two methods are
 *    deliberately kept as separate near-duplicates rather than parameterised: every line
 *    differs by which operand it names, and a shared version would be one big
 *    `if (isReuseA)`.
 *  - `buildReuseC` — neither operand is held; the PARTIAL SUM is. See below.
 *
 * Output-stationary (`buildReuseC`) is the intricate one. The accumulator feeds back into
 * itself while `feedbackEnable` is high and is flushed to the C-chain on that signal's
 * FALLING edge (`drainPulse`). The subtlety is that every physical row must fall on the
 * SAME cycle — otherwise the vertical chain, which shifts one row per cycle, would
 * collect rows that finished at different times. The three branches computing
 * `feedbackEnable` are three ways to say that one thing, one per fabric shape, and the
 * cluster branch is the one that cannot use [[AccEnableDelaySupport]]: it aligns on the
 * GLOBAL row `r * lvR + a`, giving a PE at row g a delay of `(totalRows - g)` so that
 * (chain position g) + (delay) is constant. For `lvR == 1` it reduces to the standalone
 * form, which is the check that the two agree.
 *
 * Dual mode adds a second input lane muxed in AT THE MAC — that mux is the entire
 * feature at this level. The second stream needs no skew (it arrives aligned, one
 * dedicated lane per PE) and never touches the C-chain; its result leaves through the
 * `outputPartial` tap for the array-level [[DualModeAdderTree]]. So it costs one mux and
 * one tap, and reuses the multiplier and the tree.
 */
class VectorPE[InputType <: Data, AccType <: Data](
  val outerIndex: OuterPeIndex,
  val innerIndex: InnerIndex,
  val vectorPortMask: PortEnableMask,
  val config: SystolicArrayConfig,
  val dualMode: Boolean = false,
)(
 implicit val arithmetic: Arithmetic[InputType, AccType],
 implicit val portType: PortTypeProvider[InputType, AccType],
) extends Component
  with ProcessingElementIO[InputType, AccType]
  with AdderTreeSupport[AccType]
  with AccEnableDelaySupport {

  import arithmetic._
  private val topology = config.topology

  // ReuseC clusters chain their inner sub-rows' C ports to drain output-stationary
  // results (see buildReuseC / ClusterPE.wireC), so inputC is legal on non-standalone
  // ReuseC inner PEs. Every other dataflow reduces spatially and only the standalone
  // path carries inputC.
  if(vectorPortMask.withInputPortC && !config.isReuseC) {
    config.requireStandAlone()
  }

  // Dual-mode second input stream, injected straight at the MAC. Each VectorPE gets
  // its OWN lane (N-to-N, no systolic reuse) on the STREAMED operand, and a select
  // time-shares the single MAC between the stock stream and it. The lane arrives
  // fully aligned and is consumed as-is — second-stream partial sums leave through
  // the `outputPartial` tap into the array-level DualModeAdderTree, not the C-chain,
  // so no per-PE skew exists. ReuseA and ReuseB are mirror images: ReuseB holds B
  // stationary and streams A (`inputA2`/`selectA`), ReuseA holds A stationary and
  // streams B (`inputB2`/`selectB`).
  private val dualModeReuseA: Boolean = dualMode && config.isReuseA
  private val dualModeReuseB: Boolean = dualMode && config.isReuseB

  // The dot-product tap (and the array-level DualModeAdderTree it feeds) only exists
  // when the contraction axis spans MORE than one outer PE. With a single outer hop
  // the whole axis reduces through the intra-cluster adder tree and the stock chain
  // output already IS the dual result — see ToroidalSystolicArray.muxDualModeOutputs.
  private val dualModeTree: Boolean =
    (dualModeReuseB && config.outerPeRow > 1) || (dualModeReuseA && config.outerPeCol > 1)

  val io = new Bundle {

    //Inputs
    val inputA = in Vec(portType.createInputTypeA, config.multPerPe)
    val inputB = in Vec(portType.createInputTypeB, config.multPerPe)

    val inputA2 = dualModeReuseB generate { in Vec(portType.createInputTypeA, config.multPerPe) }
    val selectA = dualModeReuseB generate { in Bool() }

    val inputB2 = dualModeReuseA generate { in Vec(portType.createInputTypeB, config.multPerPe) }
    val selectB = dualModeReuseA generate { in Bool() }

    val inputC = vectorPortMask.withInputPortC generate {
      in Vec(portType.createVectorPeInputTypeC(outerIndex, innerIndex), 1)
    }

    val inputUpdateEnableA = config.isReuseA generate {in Vec(Bool(), 1)}
    val inputUpdateEnableB = config.isReuseB generate {in Vec(Bool(), 1)}
    val inputAccEnableC = config.isReuseC generate {in Vec(Bool(), 1)}
    val accControlInput = topology.needVectorPeAccControlPort(outerIndex, innerIndex) generate {in Vec(AccControl(), 1)}

    //Outputs
    val outputA = vectorPortMask.withOutputPortA generate {
      out Vec(portType.createInputTypeA, config.multPerPe)
    }
    val outputB = vectorPortMask.withOutputPortB generate {
      out Vec(portType.createInputTypeB, config.multPerPe)
    }
    val outputC = out Vec(portType.createVectorPeOutputTypeC(outerIndex, innerIndex), 1)

    // Dual mode with a multi-outer-PE contraction axis only: this PE's locally
    // reduced dot product (raw adder-tree output, before any resize), tapped for the
    // array-level DualModeAdderTree.
    val outputPartial = dualModeTree generate {
      out Vec(portType.createVectorPeDotProductType, 1)
    }


    val outputUpdateEnableA = (config.isReuseA && vectorPortMask.withOutputPortA) generate {out Vec(Bool(), 1)}
    val outputUpdateEnableB = (config.isReuseB && vectorPortMask.withOutputPortB) generate {out Vec(Bool(), 1)}
    val outputAccEnableC = (config.topology.needAccEnableC(outerIndex) && config.isStandAloneVectorPeConfig) generate {
      out Vec(Bool(), 1)
    }

    val accControlOutput = topology.needVectorPeAccControlPort(outerIndex, innerIndex) generate {out Vec(AccControl(), 1)}

  }

  config.dataflow match {
    case Dataflow.ReuseA =>
      buildReuseA()
    case Dataflow.ReuseB =>
      buildReuseB()
    case Dataflow.ReuseC =>
      buildReuseC()
  }

  // The stationary operand: hold the last value clocked in by the enable pulse. The
  // enable travels alongside the data down the same chain, so a PE captures exactly when
  // its own element arrives rather than on a globally timed cycle.
  private def captureInput(input: Vec[InputType], inputCaptureEnable: Vec[Bool], zeroValue: InputType): Vec[InputType] = {
    Vec(input.map { elem =>
      RegNextWhen(
        next = elem,
        cond = inputCaptureEnable(0),
        init = zeroValue,
      )
    })
  }

  private def registerInput(input: Vec[InputType], zeroValue: InputType): Vec[InputType] = {
    Vec(input.map { elem =>
      RegNext(
        next = elem,
        init = zeroValue
      )
    })
  }

  // One multiplier per lane, each with an output register. That register is the `1` in
  // AccEnableDelaySupport's delay formula; adding or removing a stage here silently
  // changes every ReuseC drain alignment in the fabric.
  private def multiply(inputA : Vec[InputType], inputB : Vec[InputType]): Vec[AccType] = {
    Vec(inputA.zip(inputB).map{ case (a,b) => RegNext( a * b , init = portType.zeroMultOutput) })
  }

  private def buildReuseA(): Unit = {

    val zero = portType.zeroVectorPeOutputTypeC(outerIndex, innerIndex)
    val capturedA = captureInput(io.inputA, io.inputUpdateEnableA, portType.zeroInputA)

    // In dual mode the MAC's B operand is muxed between the stock systolic stream
    // (`inputB`) and this PE's dedicated dual-mode lane (`inputB2`). Mirror of the
    // ReuseB path in buildReuseB.
    val macInputB = if (dualModeReuseA) Mux(io.selectB, io.inputB2, io.inputB) else io.inputB
    val products = multiply(capturedA, macInputB)

    val dotProduct = adderTree(products)
    if (dualModeTree) io.outputPartial(0) := dotProduct
    val outputType = portType.createVectorPeOutputTypeC(outerIndex, innerIndex)

    if(vectorPortMask.withOutputPortA) {
      io.outputA := RegNext(capturedA, init = Vec.fill(config.multPerPe)(portType.zeroInputA))
      io.outputUpdateEnableA := RegNext(io.inputUpdateEnableA, Vec.fill(1)(False))
    }

    if(vectorPortMask.withOutputPortB)
      io.outputB := registerInput(io.inputB, portType.zeroInputB)

    // Who owns the C-chain register. On the array (a chain hop of its own, or the top of
    // the chain) this PE registers; inside a cluster the enclosing OuterPE's
    // row/colWiseAccumulate already registers the reduced sum, so adding one here would
    // put this PE a cycle behind its siblings.
    if(vectorPortMask.withInputPortC){
      io.outputC(0) := RegNext((dotProduct + io.inputC(0)).resizeTo(outputType), init = zero)
    } else if (config.isStandAloneVectorPeConfig){
      io.outputC(0) := RegNext(dotProduct.resizeTo(outputType), init = zero)
    } else {
      io.outputC(0) := dotProduct.resizeTo(outputType)
    }

    // The accumulator handshake is delayed to match this PE's datapath: the extra cycle
    // in the standalone case is the C-chain register the branch above just added.
    if(topology.needVectorPeAccControlPort(outerIndex, innerIndex)){
      if(topology.isVectorPeAccControlSource(outerIndex, innerIndex)){
        io.accControlOutput(0) := Delay(
          io.accControlInput(0),
          (if (config.isStandAloneVectorPeConfig) 2 else 1) + topology.log2MultPerPe,
          init = AccControl.zero,
        )
      } else if(topology.isStandAloneAccControlCarrier(outerIndex)){
        io.accControlOutput(0) := Delay(io.accControlInput(0), 1, init = AccControl.zero)
      } else {
        // The port exists but this PE is neither the control source nor a carrier, so
        // the topology's three predicates disagree about it and there is nothing sane to
        // drive it with. Unreachable unless a topology gains a case one of them missed.
        throw new IllegalArgumentException(
          s"[error] accControl port generated for VectorPE ($outerIndex, $innerIndex), " +
            "but it is neither a control source nor a carrier")
      }
    }

  }

  private def buildReuseB(): Unit = {

    val zero = portType.zeroVectorPeOutputTypeC(outerIndex, innerIndex)
    val capturedB = captureInput(io.inputB, io.inputUpdateEnableB, portType.zeroInputB)

    // In dual mode the MAC's A operand is muxed between the stock systolic
    // stream (`inputA`) and this PE's dedicated dual-mode lane (`inputA2`).
    val macInputA = if (dualModeReuseB) Mux(io.selectA, io.inputA2, io.inputA) else io.inputA
    val products = multiply(macInputA, capturedB)


    val dotProduct = adderTree(products)
    if (dualModeTree) io.outputPartial(0) := dotProduct
    val outputType = portType.createVectorPeOutputTypeC(outerIndex, innerIndex)

    if(vectorPortMask.withOutputPortA)
      io.outputA := registerInput(io.inputA, portType.zeroInputA)

    if(vectorPortMask.withOutputPortB) {
      io.outputB := RegNext(capturedB, init = Vec.fill(config.multPerPe)(portType.zeroInputB))
      io.outputUpdateEnableB := RegNext(io.inputUpdateEnableB, Vec.fill(1)(False))
    }

    // Identical to buildReuseA's tail; see the comments there for who owns the C-chain
    // register and why the standalone accControl delay carries an extra cycle.
    if(vectorPortMask.withInputPortC){
      io.outputC(0) := RegNext((dotProduct + io.inputC(0)).resizeTo(outputType), init = zero)
    } else if (config.isStandAloneVectorPeConfig){
      io.outputC(0) := RegNext(dotProduct.resizeTo(outputType), init = zero)
    } else {
      io.outputC(0) := dotProduct.resizeTo(outputType)
    }

    if(topology.needVectorPeAccControlPort(outerIndex, innerIndex)){
      if(topology.isVectorPeAccControlSource(outerIndex, innerIndex)){
        io.accControlOutput(0) := Delay(
          io.accControlInput(0),
          (if (config.isStandAloneVectorPeConfig) 2 else 1) + topology.log2MultPerPe,
          init = AccControl.zero,
        )
      } else if(topology.isStandAloneAccControlCarrier(outerIndex)){
        io.accControlOutput(0) := Delay(io.accControlInput(0), 1, init = AccControl.zero)
      } else {
        throw new IllegalArgumentException(
          s"[error] accControl port generated for VectorPE ($outerIndex, $innerIndex), " +
            "but it is neither a control source nor a carrier")
      }
    }

  }

  private def buildReuseC(): Unit = {

    val zero = portType.zeroVectorPeOutputTypeC(outerIndex, innerIndex)
    val products = multiply(io.inputA, io.inputB)
    val dotProduct = adderTree(products)
    val partialSumReg = Reg(portType.createVectorPeOutputTypeC(outerIndex, innerIndex)) init zero
    val outputType = portType.createVectorPeOutputTypeC(outerIndex, innerIndex)

    if (vectorPortMask.withOutputPortA) {
      if (outerIndex.c == config.outerPeCol - 1) {
        io.outputA := io.inputA
      } else {
        io.outputA := registerInput(io.inputA, portType.zeroInputA)
      }
    }

    if (vectorPortMask.withOutputPortB) {
      if (outerIndex.r == config.outerPeRow - 1) {
        io.outputB := io.inputB
      } else {
        io.outputB := registerInput(io.inputB, portType.zeroInputB)
      }
    }

    if (config.topology.needAccEnableC(outerIndex) && config.isStandAloneVectorPeConfig) {
      io.outputAccEnableC(0) := RegNext(io.inputAccEnableC(0), False)
    }

    // High only from the SECOND cycle of the enable onward. The first cycle's product has
    // not cleared the multiplier register yet, so accumulating on it would fold in the
    // previous tile's leftover.
    val gatedAccEnable = io.inputAccEnableC(0) && RegNext(io.inputAccEnableC(0), False)

    // Output-stationary drain aligns every physical row's feedbackEnable to fall on
    // the same cycle (so all rows capture their partial sum on one shared drainPulse),
    // then lets the vertical C-chain shift them out one row per cycle.
    val feedbackEnable =
      if (config.isStandAloneVectorPeConfig) {
        // Standalone array (Grouped or Toroidal): the topology's additionalDelay already encodes
        // the per-row stagger; keep the original behavior exactly.
        delayAccEnable(gatedAccEnable, outerIndex)
      } else if (config.isClusterPeConfig) {
        // Toroidal ReuseC broadcast cluster inner PE: align by GLOBAL physical row g. A PE at
        // row g needs delay (totalRows - g) so (chain-in = g) + delay is constant across
        // rows. The ClusterPE feeds each inner row an accEnableC pre-delayed by `a`,
        // completing the same 1+g chain; for lvR == 1 this matches the standalone form.
        val totalRows = config.outerPeRow * config.localVectorPeRow
        val globalRow = outerIndex.r * config.localVectorPeRow + innerIndex.a
        Delay(gatedAccEnable, 1 + log2Up(config.multPerPe) + (totalRows - globalRow), init = False)
      } else {
        // Grouped grouped ReuseC inner PE: original ungated pass-through gate.
        io.inputAccEnableC(0)
      }

    // Accumulate against the held sum while the gate is open, against ZERO on the first
    // cycle after it opens — that is what starts a fresh output element without needing a
    // separate clear pulse.
    val feedBackValue = Mux(feedbackEnable, partialSumReg, zero)
    val accumulatedValue = dotProduct.resizeTo(outputType) + feedBackValue
    partialSumReg := accumulatedValue

    // Falling edge of the gate: the sum is final this cycle and must leave now, because
    // the next cycle's `feedBackValue` already resets it to zero.
    val drainPulse = RegNext(feedbackEnable, False) && !feedbackEnable
    if (vectorPortMask.withInputPortC) {
      val delayedInputC = RegNext(io.inputC(0), init = zero)
      io.outputC(0) := RegNext(Mux(drainPulse, partialSumReg, delayedInputC), init = zero)
    } else if (config.isStandAloneVectorPeConfig || config.isClusterPeConfig) {
      // Top of a drain chain (global row 0): standalone top row, or an Toroidal ReuseC
      // cluster's top-left inner PE. No upstream C to forward, so drain against zero.
      io.outputC(0) := RegNext(Mux(drainPulse, partialSumReg, zero), init = zero)
    } else {
      // Grouped grouped ReuseC inner PE: expose the raw partial sum (collected upstream).
      io.outputC(0) := partialSumReg
    }
  }

}