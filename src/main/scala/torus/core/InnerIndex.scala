package torus.core

/**
 * Where a VectorPE sits INSIDE its cluster — the fine half of a PE's address, always
 * read together with the [[OuterPeIndex]] that names the cluster.
 *
 * `a` / `b` range over `localVectorPeRow × localVectorPeCol`, the cluster's own extent,
 * and are deliberately not called `r` / `c`: the pair `(a, b)` is what tells an inner
 * position apart from an outer one at a call site that takes both, as
 * `ArrayTopology.needVectorPeAccControlPort` and `PortTypeProvider.createVectorPeInputTypeC`
 * do.
 *
 * `InnerIndex(0, 0)` is the degenerate case, not a special one: an unclustered fabric
 * (`SystolicArray.createVectorPe`) puts one VectorPE at each outer position and gives it
 * this index, so the same topology and port-type queries answer for both fabrics without
 * a separate stand-alone path.
 *
 * Non-negativity is checked here; the cluster extent is the config's to enforce.
 */
case class InnerIndex(a: Int, b: Int) {
  require(a >= 0, s"Row index must be non-negative, got $a")
  require(b >= 0, s"Column index must be non-negative, got $b")
}
