package torus.array

import spinal.core._

import torus.arithmetic._
import torus.buffer._
import torus.config._
import torus.core._
import torus.pe._

/**
 * The classic (non-toroidal) fabric: an `outerPeRow` x `outerPeCol` mesh with LINEAR
 * operand chains — A walks west to east, B walks north to south, and neither end wraps.
 *
 * Because the chains are linear, an operand reaches cluster column `c` only `c` cycles
 * after it entered, so the array cannot be fed a rectangular block of data. The stagger
 * is manufactured at the BOUNDARY instead, which is what shapes this whole class:
 * {{{
 *   in    streamed operand  -> SkewBuffer     (group i held i+1 cycles)
 *         stationary operand -> RegisterBuffer (one flat cycle; it is captured, not streamed)
 *   out   C                 -> SkewBuffer, DE-skew direction (group i held depth-i)
 * }}}
 * The exit de-skew is not symmetry for its own sake: it is what leaves every output port
 * presenting the same output element on the same cycle, which is the precondition
 * [[AccTap]] and the single-address [[AccSramDoubleBuffer]] are built on. Compare
 * [[ToroidalSystolicArray]], which needs neither buffer because its wrap supplies the
 * stagger inside the fabric — and which therefore has no de-skew and must earn the same
 * alignment a different way.
 *
 * WHICH operand is stationary mirrors with the dataflow, and the buffer choice mirrors
 * with it:
 * {{{
 *             stationary   streamed      C chain direction
 *   ReuseA        A         B            west -> east   (per sub-row)
 *   ReuseB        B         A            north -> south (per sub-column)
 *   ReuseC      neither    both          north -> south, drains finished elements
 * }}}
 *
 * ReuseC is the odd one out throughout: it is output-stationary, so its C chain shifts
 * FINISHED elements out rather than accumulating along the way, and it never has an
 * external accumulator.
 *
 * All wiring is inherited from [[SystolicArray]]; this class only decides which helper
 * each dataflow gets and what sits at the edges.
 */
class GroupedSystolicArray[InputType <: Data, AccType <: Data](
  override val config: SystolicArrayConfig
)(
  implicit val arithmetic: Arithmetic[InputType, AccType],
  implicit val portType: PortTypeProvider[InputType, AccType],
) extends Component with SystolicArray[InputType, AccType] {

  setDefinitionName(buildDefinitionName())

  //Grouped Systolic array needs skew buffer and de-skew buffer
  // Order matters: the buffers and PEs must all exist before `wire` reads their ports.
  val preprocessorA = createPreprocessorA()
  val preprocessorB = createPreprocessorB()
  val pes = createOuterPes()
  val postProcessorC= createPostProcessorC()

  wire(pes)

  /**
   * A 1x1 cluster (`localVectorPeRow == localVectorPeCol == 1`) would be a GroupPE
   * wrapping exactly one VectorPE, so the wrapper is skipped and the VectorPE is placed
   * directly in the mesh. Both satisfy `ProcessingElementIO`, so nothing downstream
   * branches on it — but the generated netlist has one fewer level of hierarchy, and the
   * topology's `standAloneVectorPePortMask` exists for precisely this case.
   */
  private def createOuterPes(): Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]] = {
    if (config.isStandAloneVectorPeConfig) {
      createVectorPe()
    } else {
      Vector.tabulate(config.outerPeRow, config.outerPeCol) { (r, c) =>
        new GroupPE(
          outerPeIndex = OuterPeIndex(r, c),
          portMask = topology.outerPePortMask(OuterPeIndex(r, c)),
          config = config
        )
      }
    }
  }

  // A is STATIONARY under ReuseA — captured into the PEs by `inputUpdateEnableA` rather
  // than flowing with the wavefront — so a flat register stage is all it needs. Under
  // ReuseB and ReuseC it streams and must be pre-staggered.
  private def createPreprocessorA(): Component with SkewBufferIO[InputType] = config.dataflow match {
    case Dataflow.ReuseA =>
      RegisterBuffer.forInput[InputType, AccType](TileType.TypeA, config)
    case Dataflow.ReuseB | Dataflow.ReuseC =>
      SkewBuffer.forInput[InputType, AccType](TileType.TypeA, config)
  }

  // Exact mirror of createPreprocessorA on the B side.
  private def createPreprocessorB(): Component with SkewBufferIO[InputType] = config.dataflow match {
    case Dataflow.ReuseA | Dataflow.ReuseC =>
      SkewBuffer.forInput[InputType, AccType](TileType.TypeB, config)
    case Dataflow.ReuseB =>
      RegisterBuffer.forInput[InputType, AccType](TileType.TypeB, config)
  }

  // Unconditional, and unconditionally DE-skewing (`forAcc` defaults `isMinDepthFirst`
  // to false): all three dataflows drain C off a staggered edge. Its dimensions come
  // from `postBufferStep`, not the `skewBuffer*` ones — the output axis is not the
  // input axis, and the ports are whole VectorPE results with no `multPerPe` factor.
  private def createPostProcessorC(): Component with SkewBufferIO[AccType] =
      SkewBuffer.forAcc[InputType, AccType](TileType.TypeC, config)

  override def wire(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    wireA(pes)
    wireB(pes)
    wireControl(pes)
    wireC(pes)
  }

  // The edge buffer is the only thing between the port and the fabric; `systolicWireA`
  // then builds the same linear chain whichever buffer that was.
  override def wireA(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    preprocessorA.io.input := io.inputA
    systolicWireA(pes, preprocessorA.io.output(_))
  }

  override def wireB(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    preprocessorB.io.input := io.inputB
    systolicWireB(pes, preprocessorB.io.output(_))
  }

  /**
   * Exactly one control bus exists per dataflow (the others are not even generated on
   * `io`), and each propagates along the same axis as its operand.
   *
   * `applySkew = true` for ReuseC is the one asymmetry: its `accEnableC` must follow the
   * operand skew column by column, so it goes through a [[SkewBuffer]] rather than a flat
   * register. The Toroidal array passes `false` at the same call.
   */
  override def wireControl(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA => systolicWireUpdateEnableA(pes, io.inputUpdateEnableA)
      case Dataflow.ReuseB => systolicWireUpdateEnableB(pes, io.inputUpdateEnableB)
      case Dataflow.ReuseC => systolicWireAccEnableC(pes, io.inputAccEnableC, applySkew = true)
    }
  }

  override def wireC(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    connectPeChain(pes)
    connectPostProcessorToOutput(pes)
  }

  override def connectPeChain(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        connectPeChainReuseA(pes)

      case Dataflow.ReuseB =>
        connectPeChainReuseB(pes)

      case Dataflow.ReuseC =>
        // Not an accumulation chain: every VectorPE already holds a finished output
        // element, and this shifts them southwards to the drain edge. Hence the third
        // loop over ALL `numVectorPePerGroupPe` PEs of a cluster — ReuseA/ReuseB chain
        // only the lvR sub-rows / lvC sub-columns that share a reduction, but here each
        // PE is its own result and none may be skipped.
        for {
          r <- 1 until config.outerPeRow
          c <- 0 until config.outerPeCol
          idx <- 0 until topology.numVectorPePerGroupPe
        } pes(r)(c).io.inputC(idx) := pes(r - 1)(c).io.outputC(idx)
    }
  }

  /**
   * Collect off the finished edge, de-skew, then optionally accumulate.
   *
   * Every branch routes through `postProcessorC` and then
   * [[SystolicArray.attachAccumulatorIfNeeded]], which is what makes the de-skewed ports
   * safe to hand to a single-address accumulator. Only one `accControl` element is passed
   * because Grouped's topology declares `numAccControl == 1`: the de-skew leaves all ports
   * on one timeline, so the AccTap derives every port's control from that single copy.
   */
  override def connectPostProcessorToOutput(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        collectReuseAOutputs(pes, postProcessorC.io.input)
        attachAccumulatorIfNeeded(
          collected           = postProcessorC.io.output,
          collectedAccControl = if (config.needAccumulator) Vec(io.accControl(0)) else null,
          outputC             = io.outputC,
        )

      case Dataflow.ReuseB =>
        collectReuseBOutputs(pes, postProcessorC.io.input)
        attachAccumulatorIfNeeded(
          collected           = postProcessorC.io.output,
          collectedAccControl = if (config.needAccumulator) Vec(io.accControl(0)) else null,
          outputC             = io.outputC,
        )

      case Dataflow.ReuseC =>
        // Output-stationary: the contraction already happened inside the PEs, so there is
        // nothing for an external accumulator to sum. Asserted here rather than left to
        // `attachAccumulatorIfNeeded` (which would simply pass through) so a config that
        // asks for one fails loudly instead of getting silently ignored hardware.
        require(
          !config.needAccumulator,
          "[error] Grouped ReuseC dose not need accumulator",
        )
        collectReuseCOutputs(pes, postProcessorC.io.input)
        // Always a pass-through given the require above; kept in the same shape as the
        // other two branches so the three stay diffable.
        attachAccumulatorIfNeeded(
          collected           = postProcessorC.io.output,
          collectedAccControl = if (config.needAccumulator) Vec(io.accControl(0)) else null,
          outputC             = io.outputC,
        )
    }
  }

}
