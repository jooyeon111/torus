package torus.buffer

import spinal.core._

import torus.accumulator._
import torus.arithmetic._
import torus.config._
import torus.core._

/**
 * One flat register stage across a whole bus — every port delayed by exactly one cycle,
 * no port-to-port stagger. The simplest [[SkewBufferIO]].
 *
 * Contents are one line; the reason it is a COMPONENT rather than an inline `RegNext`
 * at the use site is the whole point. Inline flops elaborate as loose auto-named
 * registers (`_zz_io_inputUpdateEnableB_0_4`), scattered through the array's own body;
 * routing them through a named module instead gives synthesis one uniform, addressable
 * register stage per array input, and gives the netlist a name a human can find. That
 * is why the `object` below has a factory per bus kind rather than one generic one —
 * the name is the deliverable.
 *
 * Every factory except [[RegisterBuffer.forDualStream]] replaced flops that already
 * existed, so they are structural no-ops: same one cycle, same zero init. `forDualStream`
 * is the exception and ADDS a cycle — its own doc says so.
 *
 * `inputType` and `zero` are BY-NAME (`=> T`) so each of the `numPort` elements gets a
 * fresh hardware type / literal; passing one shared instance into a `Vec` is a
 * SpinalHDL elaboration error.
 *
 * @param bufferName definition name of the generated module — this is what makes the
 *                   stage findable in the netlist, so it is required, not derived.
 */
class RegisterBuffer[T <: Data](
  val bufferName: String,
  inputType: => T,
  zero: => T,
  val numPort: Int,
) extends Component with SkewBufferIO[T]{

  setDefinitionName(bufferName)

  override val io = new Bundle {
    val input  = in  Vec(HardType(inputType), numPort)
    val output = out Vec(HardType(inputType), numPort)
  }

  // Vec-wide RegNext: one flop per port, all with the same typed init, so the bus comes
  // out of reset holding zeros rather than X.
  io.output := RegNext(io.input, Vec.fill(numPort)(zero))

}

object RegisterBuffer {

  /**
   * Entry stage for an OPERAND bus that needs no stagger — the stationary operand of a
   * Grouped array (A for ReuseA, B for ReuseB), and both operands of a Toroidal
   * ReuseA/ReuseB array, whose wrap wiring supplies the stagger inside the fabric.
   * The streamed Grouped operand gets a [[SkewBuffer]] at this same slot instead.
   */
  def forInput[InputType <: Data, AccType <: Data](
    tileType: TileType.Value,
    config: SystolicArrayConfig,
  )(
    implicit portType: PortTypeProvider[InputType, AccType],
    arithmetic: Arithmetic[InputType, AccType],
  ): RegisterBuffer[InputType] = {
    val topology = config.topology
    new RegisterBuffer[InputType](
      bufferName = s"${tileType}_SyncBuffer",
      inputType  = portType.createInputTypeByTile(tileType),
      zero       = portType.zeroInputByTile(tileType),
      numPort    = topology.syncBufferNumPort(tileType),
    )
  }

  /**
   * Registered entry stage for the external accumulator's control bus, so it crosses
   * the array boundary through the same buffer module the operand buses use instead of
   * an inline `RegNext`. Purely structural — one cycle with a zero init either way —
   * but it gives synthesis one uniform, nameable register stage on every array input
   * rather than loose flops scattered at the top level.
   *
   * Only meaningful for a ReuseA / ReuseB accumulator config: ReuseC has no external
   * accumulator and its topology has no `numAccControl` to size the bus from.
   */
  def forAccControl(config: SystolicArrayConfig): RegisterBuffer[AccControl] = {
    require(config.needAccumulator,
      "[error] RegisterBuffer.forAccControl is only for configs with an external accumulator")
    new RegisterBuffer[AccControl](
      bufferName = "AccControl_SyncBuffer",
      inputType  = AccControl(),
      zero       = AccControl.zero,
      numPort    = config.topology.numAccControl,
    )
  }

  /**
   * Registered entry stage for a 1-bit control bus (weight-capture enables, mode
   * selects). Same role as [[forAccControl]]: the flops already existed as inline
   * `RegNext`s in the parent, which synthesis sees as loose auto-named registers
   * (`_zz_io_inputUpdateEnableB_0_4`); routing them through a buffer module gives one
   * uniform, addressable register stage per array input instead.
   *
   * Purely structural — one cycle with a `False` init, exactly what the inline flops
   * gave.
   */
  def forControl(bufferName: String, numPort: Int): RegisterBuffer[Bool] =
    new RegisterBuffer[Bool](
      bufferName = bufferName,
      inputType  = Bool(),
      zero       = False,
      numPort    = numPort,
    )

  /**
   * Registered entry stage for a per-VectorPE dual-mode second stream. Unlike the
   * control/accControl buffers this ADDS a cycle the design did not have: the lanes
   * used to run from the top-level port straight into each MAC's mux (the outer-index-0
   * PEs get `Delay(_, 0)`), leaving a port -> mux -> multiplier combinational path. The
   * added stage also makes the dual path exactly as deep as the stock operand path,
   * which crosses its own input buffer — see [[ToroidalSystolicArray]].
   */
  def forDualStream[InputType <: Data, AccType <: Data](
    tileType: TileType.Value,
    numPort: Int,
  )(
    implicit portType: PortTypeProvider[InputType, AccType],
    arithmetic: Arithmetic[InputType, AccType],
  ): RegisterBuffer[InputType] =
    new RegisterBuffer[InputType](
      bufferName = s"${tileType}2_DualStream_SyncBuffer",
      inputType  = portType.createInputTypeByTile(tileType),
      zero       = portType.zeroInputByTile(tileType),
      numPort    = numPort,
    )
}
