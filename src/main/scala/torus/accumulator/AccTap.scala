package torus.accumulator

import spinal.core._
import spinal.lib.Delay

import torus.arithmetic._
import torus.config._

/**
 * Re-times the accumulator control to match when each output port's psums actually
 * arrive, so one [[AccControl]] can drive every port of the shared-address
 * [[AccSramDoubleBuffer]].
 *
 * Grouped only. The TypeC de-skew buffer upstream already aligns the DATA; this aligns
 * the CONTROL to it. The data path through here is a pure passthrough — `io.output` is
 * `io.input` unchanged — so nothing in this module compensates a skew in the data.
 *
 * Structurally it is a chain of [[AccTapCell]]s, one per `numAccTapCellPort` ports, with
 * a `accTapPreludeDelay` prelude in front. Functionally the whole chain is one constant
 * delay: each cell's own delay subtracts exactly the `cellIndex` cycles the chain spent
 * reaching it, so every port's `delayedAccControl` lands on the same cycle,
 * `accTapPreludeDelay + accTapCellDepth + numCells` after `accControlIn`. The per-cell
 * structure is about DISTRIBUTION — one register hop per cell rather than a broadcast
 * across `systolicOutputDimC` ports — not about differing per-port latency.
 *
 * That constant, equal arrival is the aligned-ports precondition [[AccSram]] states and
 * cannot check. Retiming either delay in isolation de-aligns the ports silently.
 */
class AccTap[InputType <: Data, AccType <: Data](
  val config: SystolicArrayConfig
)(
  implicit val arithmetic: Arithmetic[InputType, AccType],
  implicit val portType: PortTypeProvider[InputType, AccType],
) extends Component {

  // Toroidal reaches the accumulator with uniform per-port latency and needs no tap;
  // Grouped ReuseC is output-stationary and has no accumulator at all.
  config.requireGrouped()
  private val topology = config.topology
  private val portsPerCell = topology.numAccTapCellPort
  private val numCells     = topology.systolicOutputDimC / portsPerCell

  require(
    topology.systolicOutputDimC % portsPerCell == 0,
    s"[error] systolicOutputDimC(${topology.systolicOutputDimC}) must be divisible by " +
      s"numAccTapCellPort($portsPerCell)"
  )

  val io = new Bundle {
    val input             = in  Vec(portType.createSystolicOutputTypeC, topology.systolicOutputDimC)
    val accControlIn      = in  (AccControl())
    val output            = out Vec(portType.createSystolicOutputTypeC, topology.systolicOutputDimC)
    val delayedAccControl = out Vec(AccControl(), topology.systolicOutputDimC)
  }

  // Prelude: the de-skew buffer's own step, spent before the first cell sees the control.
  private val preludedControl = Delay(
    io.accControlIn,
    topology.accTapPreludeDelay,
    init = AccControl.zero
  )

  val cells = Vector.tabulate(numCells) { i =>
    new AccTapCell(
      cellIndex        = i,
      numCells         = numCells,
      isLastAccTapCell = i == numCells - 1,
      config           = config,
    )
  }

  // Daisy-chain: cell 0 takes the prelude, each later cell takes its predecessor's
  // one-cycle-delayed copy. This is the `+cellIndex` that each cell then cancels.
  for (i <- 0 until numCells) {
    if (i == 0)
      cells(0).io.accControlIn := preludedControl
    else
      cells(i).io.accControlIn := cells(i - 1).io.accControlOutput
  }

  // Flatten cell-local port indices back onto the array's `systolicOutputDimC` ports.
  for (i <- 0 until numCells; p <- 0 until portsPerCell) {
    val flatIdx = i * portsPerCell + p
    cells(i).io.input(p)            := io.input(flatIdx)
    io.output(flatIdx)              := cells(i).io.output(p)
    io.delayedAccControl(flatIdx)   := cells(i).io.delayedAccControl(p)
  }

}