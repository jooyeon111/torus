package torus.buffer

import spinal.core._

import torus.arithmetic._
import torus.config._
import torus.core._

/**
 * External demux on the STATIONARY operand's port for Toroidal DualMode, with a
 * `select` for normal mode.
 *
 * Which operand is stationary mirrors with the dataflow: ReuseB holds B (the classic
 * weight-stationary case) and broadcasts across COLUMN groups; ReuseA holds A and
 * broadcasts across ROW groups. In low-m GEMV that stationary tile (physically held in
 * an SRAM) is small, so the same group is copied and loaded into every group's
 * capture module inside the systolic array.
 *
 * The array puts this module in front of the stationary operand's own port — `inputB`
 * (ReuseB) or `inputA` (ReuseA) — and it produces the full-width stationary bus the
 * fabric then loads (see `ToroidalSystolicArray.stationaryOperand`). The `select` picks
 * between two modes on the SAME full-width `input` port (one wide port, shared to save
 * hardware):
 *
 *   - select = broadcast : copy the first VMU group (`input(0..groupWidth)`)
 *                          across every group. Only group 0 of `input` is
 *                          meaningful; the rest is ignored.
 *   - select = normal    : pass the full-width bus straight through, so each
 *                          group gets its own distinct stationary values.
 *
 * The in-array capture modules (the `inputUpdateEnableA`/`inputUpdateEnableB`-gated
 * capture in VectorPE) are unchanged in both modes.
 */
class StationaryDemux[T <: Data](
  inputType: => T,
  val groupWidth: Int,
  val numGroups: Int,
) extends Component {

  require(groupWidth >= 1, s"[error] StationaryDemux groupWidth must be >= 1, got $groupWidth")
  require(numGroups >= 1, s"[error] StationaryDemux numGroups must be >= 1, got $numGroups")

  setDefinitionName("Stationary_DualModeDemux")

  val outputWidth: Int = groupWidth * numGroups

  val io = new Bundle {
    val input  = in  Vec(HardType(inputType), outputWidth)
    val select = in  Bool()   // True -> broadcast, False -> normal passthrough
    val output = out Vec(HardType(inputType), outputWidth)
  }

  // Broadcast : copy source group 0 into every group of the broadcast axis.
  // Normal    : pass the full-width bus through unchanged.
  for {
    g <- 0 until numGroups
    p <- 0 until groupWidth
  } io.output(g * groupWidth + p) := Mux(io.select, io.input(p), io.input(g * groupWidth + p))

}

object StationaryDemux {

  /**
   * Build the stationary-operand demux for a Toroidal DualMode config.
   *
   * The two dual-mode dataflows mirror: ReuseB holds B stationary and broadcasts it
   * across the array's COLUMN groups (bus width `systolicArrayColDimension`, B type);
   * ReuseA holds A stationary and broadcasts it across the ROW groups (bus width
   * `systolicArrayRowDimension`, A type).
   *
   * Source group  = one vertical multiplier group (`multPerPe`).
   * Output width  = the array's stationary bus, i.e. that many copies of the group.
   */
  def forStationary[InputType <: Data, AccType <: Data](
    config: SystolicArrayConfig,
  )(
    implicit portType: PortTypeProvider[InputType, AccType],
    arithmetic: Arithmetic[InputType, AccType],
  ): StationaryDemux[InputType] = {
    require(config.isReuseAB,
      s"[error] StationaryDemux is only for the dual-mode dataflows (ReuseA / ReuseB), got ${config.dataflow}")

    // Derived directly from config (not topology) so the demux does not depend on the
    // DualMode topology being wired into ArrayTopology.apply.
    val groupWidth = config.multPerPe
    val stationaryIsA = config.isReuseA
    val outputWidth = config.dataflow match {
      case Dataflow.ReuseB => config.systolicArrayColDimension
      case Dataflow.ReuseA => config.systolicArrayRowDimension
      case Dataflow.ReuseC =>
        throw new IllegalArgumentException("[error] DualMode does not support ReuseC")
    }
    require(outputWidth % groupWidth == 0,
      s"[error] stationary bus width ($outputWidth) must be a multiple of multPerPe ($groupWidth)")

    // `inputType` is by-name: keep the branch inline so every element gets a FRESH
    // hardware type instance rather than a shared one.
    new StationaryDemux[InputType](
      inputType  = if (stationaryIsA) portType.createInputTypeA else portType.createInputTypeB,
      groupWidth = groupWidth,
      numGroups  = outputWidth / groupWidth,
    )
  }
}
