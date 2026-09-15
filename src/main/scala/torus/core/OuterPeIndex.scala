package torus.core

/**
 * Where a cluster sits in the OUTER grid — the coarse half of a PE's address.
 *
 * The fabric is addressed at two levels, and every topology query takes one or both:
 * `OuterPeIndex(r, c)` picks the cluster out of the `outerPeRow × outerPeCol` grid, and
 * [[InnerIndex]] then picks a VectorPE inside it.
 *
 * Almost everything position-dependent is a function of this pair rather than of a flat
 * PE number, because the edges are what differ: `ArrayTopology.outerPePortMask` drops a
 * cluster's A output at the last column, `additionalDelay` skews by distance from the
 * source, and `PortTypeProvider` widens the C port with depth down the accumulation
 * chain. A cluster cannot know any of that from its own contents, only from where it is.
 *
 * `r` / `c` are grid coordinates, NOT lane counts — a 2×2 grid of 4×4 clusters has
 * `r, c ∈ [0, 2)`. Both are validated non-negative here; the upper bound belongs to the
 * config that knows the grid extent, so it is not checked.
 */
case class OuterPeIndex(r: Int, c: Int) {
  require(r >= 0, s"Row index must be non-negative, got $r")
  require(c >= 0, s"Column index must be non-negative, got $c")
}
