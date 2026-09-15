package torus.pe

import spinal.core._
import spinal.lib.Delay

import torus.config._
import torus.core._

/**
 * Delays a ReuseC accumulate-enable so it reaches the accumulator on the same cycle as
 * the product it is supposed to gate.
 *
 * `accEnableC` enters a PE at the same time as the operands, but the product does not
 * exist yet — it has to cross the multiplier register and then the adder tree. Applying
 * the raw pulse would open the accumulator a fixed number of cycles EARLY, folding in
 * whatever the tree still held from the previous tile. So the pulse is delayed by the
 * datapath's own latency, term for term:
 *
 * {{{
 *   1                          // the multiplier's output register (VectorPE.multiply)
 * + log2Up(multPerPe)          // the adder tree's pipeline levels (AdderTreeSupport)
 * + topology.additionalDelay   // where this PE sits: skew already applied upstream
 * }}}
 *
 * The first two mirror hardware in this package and must be re-derived if that hardware
 * gains or loses a stage; the third is the topology's, and is the only part that varies
 * per PE.
 *
 * Mixed into [[GroupPE]] and the standalone [[VectorPE]] path. [[ClusterPE]] deliberately
 * does NOT use it — a broadcast cluster aligns by GLOBAL physical row instead
 * (`VectorPE.buildReuseC`), which `additionalDelay` does not express.
 *
 * The scalar and `Vec` forms differ only in arity: one shared delay computed once, then
 * applied per lane, since every lane of one PE sits at the same depth.
 */
trait AccEnableDelaySupport {
  def config: SystolicArrayConfig

  private final def accEnableTotalDelay(outerPeIndex: OuterPeIndex): Int = {
    1 + log2Up(config.multPerPe) + config.topology.additionalDelay(outerPeIndex)
  }

  protected final def delayAccEnable(
    accEnable: Bool,
    outerPeIndex: OuterPeIndex,
  ): Bool =
    Delay(accEnable, accEnableTotalDelay(outerPeIndex), init = False)


  protected final def delayAccEnableVec(
    accEnable: Vec[Bool],
    outerPeIndex: OuterPeIndex,
  ): Vec[Bool] = {
    val delay = accEnableTotalDelay(outerPeIndex)
    Vec.tabulate(accEnable.length) { i =>
      Delay(accEnable(i), delay, init = False)
    }
  }

}
