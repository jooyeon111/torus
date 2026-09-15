package torus.pe

import spinal.core._

import torus.arithmetic._
import torus.config._

/**
 * The ARRAY-level reduction for dual mode's second stream — the piece that exists
 * because that stream cannot use the C-chain.
 *
 * In stock operation a partial sum walks the systolic C-chain, picking up one PE's
 * contribution per hop. Dual mode's second stream is fed to every PE at once on its own
 * lane (`inputA2` / `inputB2`, see [[VectorPE]]), so its partial sums are all ready on
 * the SAME cycle and there is no chain to walk. Each PE instead taps its raw local dot
 * product out through `outputPartial`, and this component sums the taps.
 *
 * One tree per output lane. `tapsPerLane` is the extent of the CONTRACTION axis in outer
 * PEs — rows for ReuseB, columns for ReuseA — because that is the axis whose partial
 * sums belong to one output element. Inputs arrive flat, lane-major: lane `l`'s taps are
 * `input(l * tapsPerLane + k)`, and `ClusterPE.wirePartialTaps` packs them in that order.
 *
 * Only instantiated when the contraction axis spans MORE than one outer PE. With a
 * single hop the intra-cluster tree has already reduced the whole axis and the stock
 * chain output IS the dual result, so the array muxes it directly
 * (`ToroidalSystolicArray.muxDualModeOutputs`) and this component is never built — the
 * same `dualModeTree` predicate decides both, and the `outputPartial` ports only exist
 * where it holds.
 *
 * The width `require` is the real check: taps enter raw, before any resize, so if the
 * tree's natural growth does not land exactly on the declared systolic output width, the
 * tap was taken from the wrong point in the datapath. Failing at elaboration beats
 * silently resizing a mis-tapped sum.
 */
class DualModeAdderTree[InputType <: Data, AccType <: Data](
  val config: SystolicArrayConfig,
)(
  implicit val arithmetic: Arithmetic[InputType, AccType],
  implicit val portType: PortTypeProvider[InputType, AccType],
) extends Component with AdderTreeSupport[AccType] {

  require(config.isReuseAB, "[error] DualModeAdderTree only exists for dual-mode ReuseA / ReuseB")

  val numLanes: Int    = config.topology.systolicOutputDimC
  val tapsPerLane: Int = if (config.isReuseB) config.totalVectorPeRow else config.totalVectorPeCol

  setDefinitionName(s"DualModeAdderTree_${config.dataflow}_${numLanes}x${tapsPerLane}")

  val io = new Bundle {
    val input  = in  Vec(portType.createVectorPeDotProductType, numLanes * tapsPerLane)
    val output = out Vec(portType.createSystolicOutputTypeC, numLanes)
  }

  for (lane <- 0 until numLanes) {
    val taps = Vec.tabulate(tapsPerLane)(k => io.input(lane * tapsPerLane + k))
    val sum  = adderTree(taps)
    require(
      sum.getBitsWidth == io.output(lane).getBitsWidth,
      s"[error] DualModeAdderTree width: tree=${sum.getBitsWidth}, " +
        s"systolicOutput=${io.output(lane).getBitsWidth} — dot-product tap error",
    )
    io.output(lane) := sum
  }

}
