package torus.core

/**
 * Which operand stays put in the fabric — the single axis every other structural
 * decision in the project hangs off.
 *
 * The names are written from the REUSED tile's point of view; the industry names for
 * the same three schemes are the stationary operand's:
 *
 *  - `ReuseA` — Input Stationary. A is held, B streams.
 *  - `ReuseB` — Weight Stationary. B is held, A streams.
 *  - `ReuseC` — Output Stationary. Both operands stream and the partial sum stays in
 *    the PE.
 *
 * The distinction that actually changes hardware is WHERE the GEMM's K goes, and it
 * does NOT split cleanly by dataflow — ReuseC splits again by FABRIC. Read this next to
 * `SystolicArrayConfig.contractionPassDepth` and the `isReuseC` validation block, which
 * is where each case is enforced:
 *
 *  - ReuseA / ReuseB contract SPATIALLY — K is summed down the array's adder chain, so
 *    ONE pass is fixed by an array dimension (col for ReuseA, row for ReuseB). A K
 *    larger than one pass needs an external accumulator to sum the passes, and only
 *    then does the config declare `maxContractionSize`; without an accumulator it must
 *    be 0.
 *  - Grouped ReuseC contracts TEMPORALLY — K is a cycle count, how long `accEnableC` is
 *    held, and no fabric dimension bounds cycles. This is the one case where K is purely
 *    USER-DECLARED: `maxContractionSize` is required, it alone sizes the PE output, and
 *    `contractionPassDepth` refuses to answer rather than hand out an array-derived
 *    number.
 *  - Toroidal ReuseC is output-stationary like the above but back to ARRAY-DERIVED K:
 *    the wrap recirculates the operands, so one burst carries the cluster-loop length
 *    `outerPeRow × multPerPe`, and declaring `maxContractionSize` is a hard error.
 *
 * `needAccumulator` together with ReuseC is rejected outright in every case — an
 * output-stationary PE already holds the partial sum.
 *
 * Consumed through the `isReuseA` / `isReuseB` / `isReuseC` / `isReuseAB` predicates on
 * [[torus.config.SystolicArrayConfig]] rather than by matching on this enum directly;
 * match only where all three cases genuinely differ.
 */
object Dataflow extends Enumeration {
  val ReuseA, ReuseB, ReuseC = Value
}
