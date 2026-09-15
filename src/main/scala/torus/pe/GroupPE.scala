package torus.pe

import spinal.core._

import torus.arithmetic._
import torus.config._
import torus.core._

/**
 * The [[OuterPE]] of the GROUPED fabric — one cluster of the `lvR × lvC` grid a Grouped
 * array is built from.
 *
 * Read the four `wire*` methods as one 3×4 table of primitive choices. Each row is a
 * dataflow, and the shape of the table is the point: for ReuseA and ReuseB exactly one
 * operand is chained and the other broadcast, and the choice flips between them; ReuseC
 * broadcasts both, because with the output held in place neither operand is the
 * stationary one.
 *
 * {{{
 *            wireA        wireB        wireControl            wireC
 *   ReuseA   systolic     broadcast    systolicUpdateEnableA  rowWiseAccumulate
 *   ReuseB   broadcast    systolic     systolicUpdateEnableB  colWiseAccumulate
 *   ReuseC   broadcast    broadcast    delayed accEnableC     drain-on-pulse
 * }}}
 *
 * The first two rows are entirely the trait's primitives; only ReuseC is written out
 * here, because output-stationary drain has no counterpart in the other two. Its shape:
 * `accEnableC` is delayed by the datapath's own latency ([[AccEnableDelaySupport]]) so
 * the gate meets the product it belongs to, and the falling edge of that delayed gate is
 * the `drainPulse` that hands the finished partial sum to the C-chain. Between drains the
 * cluster forwards its upstream neighbour's value instead, which is what makes the chain
 * shift results out one stage per cycle.
 *
 * Contrast [[ClusterPE]], the Toroidal counterpart: same table, but its ReuseC aligns by
 * GLOBAL physical row rather than by `additionalDelay`, and it carries the dual-mode
 * lanes. This class has no dual mode at all — that is a Toroidal-only feature.
 */
class GroupPE[InputType <: Data, AccType <: Data](
  val outerPeIndex: OuterPeIndex,
  val portMask: PortEnableMask,
  val config: SystolicArrayConfig,
)(
  implicit val arithmetic: Arithmetic[InputType, AccType],
  implicit val portType: PortTypeProvider[InputType, AccType],
) extends Component
  with OuterPE[InputType, AccType]
  with AdderTreeSupport[AccType]
  with AccEnableDelaySupport{

  require(config.isGroupPeConfig, "[error] Invalid row and col")

  private val vectorPes = createVectorPes()
  private lazy val delayedAccEnable: Vec[Bool] = delayAccEnableVec(io.inputAccEnableC, outerPeIndex)

  wire()

  def wireA(): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        systolicWireA(vectorPes)

      case Dataflow.ReuseB =>
        broadcastWireA(vectorPes)

      case Dataflow.ReuseC =>
        broadcastWireA(vectorPes)

    }
  }

  def wireB(): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        broadcastWireB(vectorPes)

      case Dataflow.ReuseB =>
        systolicWireB(vectorPes)

      case Dataflow.ReuseC =>
        broadcastWireB(vectorPes)
    }
  }



  def wireControl(): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        systolicUpdateEnableA(vectorPes)

      case Dataflow.ReuseB =>
        systolicUpdateEnableB(vectorPes)

      case Dataflow.ReuseC =>

        for {
          a <- 0 until config.localVectorPeRow
          b <- 0 until config.localVectorPeCol
        } {
          vectorPes(a)(b).io.inputAccEnableC(0) := delayedAccEnable(b)
        }

        if (config.topology.needAccEnableC(outerPeIndex))
          for (b <- 0 until config.localVectorPeCol)
            io.outputAccEnableC(b) := RegNext(io.inputAccEnableC(b), False)
    }
  }

  def wireC(): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        rowWiseAccumulate(vectorPes, outerPeIndex)

      case Dataflow.ReuseB =>
        colWiseAccumulate(vectorPes, outerPeIndex)

      case Dataflow.ReuseC =>

        val inputZero = portType.zeroOuterPeInputTypeC(outerPeIndex)
        val outputZero = portType.zeroOuterPeOutputTypeC(outerPeIndex)
        for {
          a <- 0 until config.localVectorPeRow
          b <- 0 until config.localVectorPeCol
        } {
          val index = a * config.localVectorPeCol + b

          // The FALLING edge of the delayed gate: the cycle after the last product has
          // been folded in is exactly when the partial sum is final and may leave.
          val drainPulse = RegNext(delayedAccEnable(b), False) && !delayedAccEnable(b)

          // On a drain pulse emit this PE's own sum; otherwise pass the upstream
          // cluster's value along, so the chain shifts results out one stage per cycle.
          if(portMask.withInputPortC) {
            val delayedInputC = RegNext(io.inputC(index), init = inputZero)
            io.outputC(index) := RegNext(Mux(drainPulse, vectorPes(a)(b).io.outputC(0), delayedInputC), outputZero)
          } else
            io.outputC(index) := RegNext(Mux(drainPulse, vectorPes(a)(b).io.outputC(0), outputZero), init = outputZero)
        }

    }

  }
}