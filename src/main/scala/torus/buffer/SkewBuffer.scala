package torus.buffer

import spinal.core._
import spinal.lib.Delay

import torus.arithmetic._
import torus.config._
import torus.core._

/**
 * A delay RAMP across a bus: the ports are cut into `delayDepth` consecutive GROUPS of
 * `portsPerDelay`, and group `i` is delayed by a different number of cycles. Every port
 * in a group shares one delay.
 *
 * This is how a Grouped array trades space for time at its edges. The fabric is a
 * diagonal wavefront — an operand meant for cluster column `c` must not arrive until
 * the wavefront reaches it — so the bus is launched pre-staggered (SKEW) and the
 * results, which leave equally staggered, are un-staggered on the way out (DE-SKEW).
 * One flag picks which:
 * {{{
 *   isMinDepthFirst = true    SKEW      group i -> Delay(i + 1)          1, 2, 3, ... depth
 *   isMinDepthFirst = false   DE-SKEW   group i -> Delay(depth - i)      depth, ..., 2, 1
 * }}}
 * The two are inverses and both cost `depth` cycles end to end, so a skew/de-skew pair
 * around the fabric leaves the total latency uniform across ports. The naming follows:
 * `{TileType}_SkewBuffer` vs `{TileType}_DeskewBuffer`.
 *
 * Note the minimum delay is ONE, not zero, in both directions — even the earliest group
 * crosses a register. So this also serves as the entry/exit register stage, exactly like
 * the [[RegisterBuffer]] it replaces at that slot, and swapping one for the other by
 * dataflow does not change the pipeline depth of port 0.
 *
 * Groups are laid out consecutively (`portsPerDelay * i + n`), which is what lets the
 * array index the flat bus as `outerIndex * portsPerStep + lane` with no translation.
 *
 * Toroidal ReuseA / ReuseB never instantiate this: their wrap wiring staggers the
 * operands inside the fabric, so their edges use a flat [[RegisterBuffer]] instead.
 *
 * `inputType` / `zero` are by-name so every port gets a fresh hardware instance.
 */
class SkewBuffer [T <: Data](
  val bufferName: String,
  inputType: => T,
  zero: => T,
  val delayDepth : Int,
  val portsPerDelay: Int = 1,
  val isMinDepthFirst: Boolean = true
) extends Component with SkewBufferIO[T] {

  setDefinitionName(bufferName)

  override val io = new Bundle {
    val input  = in  Vec(HardType(inputType), delayDepth * portsPerDelay)
    val output = out Vec(HardType(inputType), delayDepth * portsPerDelay)
  }

  // SKEW: the ramp climbs with the group index, so group 0 (the array edge the
  // wavefront reaches first) is held the least.
  if(isMinDepthFirst){
    for ( i <- 0 until delayDepth) {
      for ( n <- 0 until portsPerDelay)
        io.output(portsPerDelay*i + n) := Delay(io.input(portsPerDelay*i + n), i + 1, init = zero)
    }
  } else {
    // DE-SKEW: the same ramp reversed. Group i already ran i cycles AHEAD coming out of
    // the fabric, so holding it `depth - i` re-aligns every group onto one cycle — which
    // is the aligned-ports precondition the AccTap / AccSram accumulator path needs.
    for( i <- 0 until delayDepth ) {
      for (n <- 0 until portsPerDelay)
        io.output(portsPerDelay*i + n) := Delay(io.input(portsPerDelay*i + n), delayDepth - i, init = zero)
    }
  }


}

object SkewBuffer {

  // The flag is the only thing that distinguishes the two directions in hardware, so it
  // also names the module — a `TypeC_SkewBuffer` in the netlist would mean the output
  // path was built with the wrong direction.
  private def nameOf(tileType: TileType.Value, isMinDepthFirst: Boolean): String =
    s"${tileType}_${if (isMinDepthFirst) "Skew" else "Deskew"}Buffer"

  /**
   * Entry skew for the STREAMED operand of a Grouped array (B for ReuseA, A for ReuseB,
   * both for ReuseC). Depth is the number of cluster hops along that operand's axis and
   * each step covers one cluster's worth of lanes, so port `outerIndex * portsPerStep +
   * lane` is held exactly as many cycles as the wavefront takes to reach it.
   */
  def forInput[InputType <: Data, AccType <: Data](
    tileType: TileType.Value,
    config: SystolicArrayConfig,
    isMinDepthFirst: Boolean = true,
  )(
    implicit portType: PortTypeProvider[InputType, AccType],
    arithmetic: Arithmetic[InputType, AccType],
  ): SkewBuffer[InputType] = {
    val topology = config.topology
    new SkewBuffer[InputType](
      bufferName      = nameOf(tileType, isMinDepthFirst),
      inputType       = portType.createInputTypeByTile(tileType),
      zero            = portType.zeroInputByTile(tileType),
      delayDepth      = topology.skewBufferDepth(tileType),
      portsPerDelay   = topology.skewBufferPortsPerStep(tileType),
      isMinDepthFirst = isMinDepthFirst,
    )
  }

  /**
   * Exit DE-skew for the C outputs of a Grouped array — note `isMinDepthFirst` defaults
   * to FALSE here, the opposite of [[forInput]], because this undoes a stagger rather
   * than creating one.
   *
   * Dimensions come from `postBufferStep` / `postBufferPortsPerStep` rather than the
   * `skewBuffer*` ones: the output axis is not the input axis (ReuseA skews B across
   * columns but drains C across rows), and the ports are whole VectorPE results, so
   * there is no `multPerPe` factor.
   *
   * Its output is what [[AccTap]] then re-times the CONTROL for; data alignment is
   * already finished here.
   */
  def forAcc[InputType <: Data, AccType <: Data](
    tileType: TileType.Value,
    config: SystolicArrayConfig,
    isMinDepthFirst: Boolean = false,
  )(
    implicit portType: PortTypeProvider[InputType, AccType],
    arithmetic: Arithmetic[InputType, AccType],
  ): SkewBuffer[AccType] = {
    val topology = config.topology
    new SkewBuffer[AccType](
      bufferName      = nameOf(tileType, isMinDepthFirst),
      inputType       = portType.createAccTypeByTile(tileType),
      zero            = portType.zeroAccTypeByTile(tileType),
      delayDepth      = topology.postBufferStep,
      portsPerDelay   = topology.postBufferPortsPerStep,
      isMinDepthFirst = isMinDepthFirst,
    )
  }

  /**
   * Registered entry stage for a 1-bit control bus whose delay GROWS along the bus —
   * the Grouped ReuseC `accEnableC`, where column `c` must be delayed by `c + 1` to
   * follow the operand skew. That is precisely this buffer's `isMinDepthFirst` law
   * (`Delay(input(portsPerDelay*i + n), i + 1)`), so replacing the inline per-column
   * `Delay` with it is structural only — same delay per port, now in one named module.
   */
  def forControl(
    bufferName: String,
    delayDepth: Int,
    portsPerDelay: Int,
  ): SkewBuffer[Bool] =
    new SkewBuffer[Bool](
      bufferName      = bufferName,
      inputType       = Bool(),
      zero            = False,
      delayDepth      = delayDepth,
      portsPerDelay   = portsPerDelay,
      isMinDepthFirst = true,
    )
}