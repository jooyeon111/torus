package torus.accumulator

import spinal.core._
import spinal.lib.Delay

import torus.arithmetic._
import torus.config._

/**
 * One slice of the [[AccTap]] control chain, covering `numAccTapCellPort` output ports.
 *
 * The DATA path is a pure passthrough (`output := input`) — this cell delays CONTROL
 * only. It does two independent things:
 *
 *  - forwards `accControlIn` one cycle down the chain to the next cell
 *    (`accControlOutput`), which is what makes the control walk across the output edge
 *    instead of fanning out to every port from one point;
 *  - emits `delayedAccControl` for its own ports, delayed to match when this cell's psums
 *    actually reach the accumulator.
 *
 * The two delays are tuned to cancel: the chain adds `cellIndex` cycles before this cell
 * sees the control, and `deskewCompensation` subtracts the same `cellIndex` from its own
 * delay. Every cell therefore emits on the SAME cycle, `accTapCellDepth + numCells` after
 * the chain's input — which is the aligned-ports precondition [[AccSram]] needs. Change
 * either delay without the other and the ports silently de-align.
 */
class AccTapCell[InputType <: Data, AccType <: Data](
  val cellIndex: Int,
  val numCells: Int,
  val isLastAccTapCell: Boolean = false,
  val config: SystolicArrayConfig,
)(
  implicit val arithmetic: Arithmetic[InputType, AccType],
  implicit val portType: PortTypeProvider[InputType, AccType],
) extends Component {

  val grouped = config.asGrouped
  val topology = config.topology

  val io = new Bundle {

    val accControlIn = in (AccControl())
    val input = in Vec(portType.createSystolicOutputTypeC, topology.numAccTapCellPort)

    val accControlOutput = !isLastAccTapCell generate out (AccControl())
    val delayedAccControl = out Vec(AccControl(), topology.numAccTapCellPort)
    val output = out Vec(portType.createSystolicOutputTypeC, topology.numAccTapCellPort)

  }

  // One register hop per cell; the last cell has no successor to feed.
  if (!isLastAccTapCell)
    io.accControlOutput := RegNext(io.accControlIn, AccControl.zero)

  // Cancels the `cellIndex` cycles the chain already spent reaching this cell, so the
  // total from the chain's input is the same constant for every cell.
  private val deskewCompensation = numCells - cellIndex

  // `accTapCellDepth` is the PE pipeline latency this cell's psums are still walking
  // (multiplier tree + GroupPE adder tree + output register); see GroupedTopology.
  val delayed = Delay(
    io.accControlIn,
    topology.accTapCellDepth + deskewCompensation,
    init = AccControl.zero
  )
  // All ports of a cell share one arrival cycle, so one delayed copy is broadcast.
  io.delayedAccControl := Vec(Seq.fill(topology.numAccTapCellPort)(delayed))

  // Data is already de-skewed upstream by the TypeC buffer — only control is re-timed here.
  io.output := io.input

}