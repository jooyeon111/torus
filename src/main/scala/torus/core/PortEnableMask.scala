package torus.core

/**
 * Which of a PE's optional ports physically exist — the edge-trimming that keeps an
 * array from elaborating wires to nowhere.
 *
 * A PE in the middle of the fabric has every port; one at an edge does not, and the
 * difference is structural, decided at elaboration rather than gated at runtime. A
 * cluster in the last column has nobody to hand A to, so `withOutputPortA` is false and
 * the port is never created. Consumers check the flag before wiring (`GroupPE` guards
 * its C input with `portMask.withInputPortC`).
 *
 * Produced only by the topologies, never by hand: `ArrayTopology.outerPePortMask` for a
 * cluster, `standAloneVectorPePortMask` for an unclustered PE, and
 * `innerVectorPePortMask` for a PE inside a cluster — the last taking the cluster's own
 * mask as input, since an inner PE may forward to a sibling even where the cluster as a
 * whole does not forward outward.
 *
 * The field names are directional and asymmetric on purpose: A and B are named by their
 * OUTPUT port, C by its INPUT. That follows the dataflow — operands are pushed forward
 * to the next PE, partial sums are pulled in from the previous one.
 */
case class PortEnableMask(
  withOutputPortA: Boolean,
  withOutputPortB: Boolean,
  withInputPortC: Boolean,
)

object PortEnableMask {

  /**
   * Two-argument form, defaulting `withInputPortC` to true.
   *
   * Currently dead — every call site in the topologies passes all three flags
   * explicitly, which is the clearer habit given that C's presence is an edge condition
   * (`outerPeIndex.c > 0`) about as often as it is unconditional. Kept as the documented
   * default rather than deleted; if it stays unused, deleting it costs nothing.
   */
  def apply(withOutputPortA: Boolean, withOutputPortB: Boolean ): PortEnableMask ={
    PortEnableMask(
      withOutputPortA = withOutputPortA,
      withOutputPortB = withOutputPortB,
      withInputPortC = true
    )
  }

}
