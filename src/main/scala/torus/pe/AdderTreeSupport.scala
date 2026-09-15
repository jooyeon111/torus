package torus.pe

import spinal.core._
import spinal.lib._

import torus.arithmetic._

/**
 * The one spatial reduction in the project: a BALANCED, PIPELINED adder tree.
 *
 * Mixed into every component that sums several partial products at once — a
 * [[VectorPE]] reducing its `multPerPe` lanes, an [[OuterPE]] reducing a cluster row or
 * column, the array-level [[DualModeAdderTree]].
 *
 * Two properties callers must budget for, because neither is visible at the call site:
 *
 *  - LATENCY is `log2Up(inputs.size)` cycles, not zero. `reduceBalancedTree`'s second
 *    argument registers every tree LEVEL, so the sum appears that many cycles after its
 *    inputs. This is the term that reappears as `log2Up(multPerPe)` in
 *    [[AccEnableDelaySupport]] and as `log2LocalVectorPeCol` / `log2LocalVectorPeRow` in
 *    `ClusterPE.wireControl`: every control pulse that must meet a sum has to be delayed
 *    by the depth of the tree that produced it. A tree whose fan-in changes silently
 *    changes those delays too.
 *  - WIDTH grows. `+^` is the expanding add, so the result is `log2Up(size)` bits wider
 *    than a leaf and nothing is ever truncated inside the tree. The resize back down to
 *    a declared port type happens at the consumer, which is why `PortTypeProvider` and
 *    not this trait owns the output widths.
 *
 * Power-of-two fan-in is required because a balanced tree has no meaning otherwise; the
 * config enforces it upstream on `multPerPe`, so a failure here means a caller built a
 * `Vec` of its own with the wrong length.
 */
trait AdderTreeSupport[AccType <: Data] {
  implicit val arithmetic: Arithmetic[_, AccType]

  import arithmetic._

  // TODO fix getBitsWidth for floating point: the per-level register is initialised from
  // the running width, which assumes an integer type whose width is its whole story.
  protected def adderTree(inputs: Vec[AccType]): AccType = {
    require(isPow2(inputs.size), "[error] adder tree input size must be power of 2")
    inputs.reduceBalancedTree(_ +^ _, (s, _) => {
      RegNext(s, init = arithmetic.zeroAccumulation(s.getBitsWidth))
    })
  }

}