package torus.buffer

import spinal.core._

import torus.arithmetic._
import torus.config._
import torus.core._

/**
 * One lane group of a [[WrapBuffer]]: a `multPerPe`-wide registered 2:1 mux that decides,
 * for one physical row (TypeA) or column (TypeB) of the torus, whether this cycle's
 * operands come from OUTSIDE the array or from the array's own far edge.
 *
 * {{{
 *   selInput = True   -> io.input        fresh operands from the array port
 *   selInput = False  -> io.wrapedInput  the far edge's output, rotated back in
 * }}}
 *
 * That mux IS the torus: with `selInput` low the row closes into a ring and the same
 * operands keep circulating, which is how Toroidal ReuseC contracts over a K longer than
 * the fabric is wide without re-fetching anything. The array drives `selInput` from
 * `accEnableC` (see `ToroidalSystolicArray.wireControl`), so fresh data is admitted
 * exactly on the cycles the PEs are told to accumulate, and the ring recirculates
 * otherwise.
 *
 * The mux output is REGISTERED, which is what makes the ring a legal sequential loop —
 * without this flop `input -> fabric -> wrapedInput -> input` would be a combinational
 * cycle. It doubles as the operand's entry register stage, so the wrap path costs the
 * same one cycle a plain [[RegisterBuffer]] would at this slot.
 *
 * All `numPort` lanes of a cell share one `selInput`: they are the `multPerPe` lanes of a
 * single physical row/column and always stream or recirculate together.
 */
class WrapBufferCell[InputType <: Data, AccType <: Data](
  val tileType: TileType.Value,
  val config: SystolicArrayConfig,
)(
  implicit val portType: PortTypeProvider[InputType, AccType],
  implicit val arithmetic: Arithmetic[InputType, AccType],
) extends Component {

  // Toroidal ReuseC only — no other dataflow recirculates operands, and `topology`
  // below throws for all of them.
  val toroidal = config.asToroidal
  toroidal.requireReuseC()

  private val topology  = config.topology
  private val numPort   = topology.wrapBufferCellPort
  // By-name: each of the `numPort` Vec elements needs its own hardware type instance.
  private def inputType = portType.createInputTypeByTile(tileType)
  private val zero      = portType.zeroInputByTile(tileType)

  val io = new Bundle {
    val input       = in  Vec(HardType(inputType), numPort)
    val wrapedInput = in  Vec(HardType(inputType), numPort)
    val selInput    = in  Bool()
    val output      = out Vec(HardType(inputType), numPort)
  }

  io.output := RegNext(Mux(io.selInput, io.input, io.wrapedInput), Vec.fill(numPort)(zero))

}
