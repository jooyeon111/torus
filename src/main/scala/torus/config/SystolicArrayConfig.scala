package torus.config

import spinal.core.isPow2

import torus.core._
import torus.topology._

/**
 * The single description of ONE array to elaborate. Every generator, driver, port type
 * and golden model reads its shape from here; nothing downstream declares a dimension
 * of its own.
 *
 * SEALED on purpose: the three case classes below are the only fabrics there are, so a
 * match on the config's class is exhaustive and the compiler names every dispatch site
 * the day a fourth is added. Branch with [[fabric]] rather than with `isGrouped` /
 * `isToroidal` — see [[isDualMode]] for why those two do not cover all three.
 *
 * The trait is three layers, in the order they appear:
 *
 *  1. DECLARED — abstract members, exactly what a config file spells out.
 *  2. DERIVED (`lazy val`) — everything computable from those, so the relation between
 *     a PE count and a multiplier-lane count is written once.
 *  3. VALIDATED — the `require` blocks, which run at construction. This is the ONLY
 *     gate a config passes: [[ConfigLoader]] transcribes without judging, so an
 *     impossible entry fails here, before elaboration, with a message naming the field.
 *
 * One rule runs through the validation and is worth stating up front: the sizing fields
 * are OWNED PER DATAFLOW. A config declares exactly the extents its own hardware
 * consumes, and declaring one it has no use for is an error rather than a harmless
 * extra — which is what stops a stale `maxContractionSize` from quietly sizing a port
 * in a machine that never reads it.
 */
sealed trait SystolicArrayConfig {

  // ── 1. Declared by the config file ──────────────────────────────────────────────

  /** Operand widths on the A and B buses. The only widths in the whole design that are
    * DECLARED; every product and partial-sum width is derived from `bitWidthA +
    * bitWidthB` by [[torus.arithmetic.PortTypeProvider]]. */
  def bitWidthA: Int
  def bitWidthB: Int

  /** Signed or unsigned. Picks the matching `Arithmetic` / `PortTypeProvider` pair, and
    * nothing else in the design branches on it. */
  def numericType: NumericType

  /** Which operand the fabric holds stationary, and therefore which axis partial sums
    * walk along. The most load-bearing field here: it changes the port widths, the
    * topology, the driver and the golden tiling all at once. */
  def dataflow: Dataflow.Value

  // The array is described at TWO levels, and that is what makes a sub-array machine
  // expressible at all:
  //
  //   outerPe{Row,Col}        OuterPEs (GroupPE / ClusterPE) laid out across the fabric
  //   localVectorPe{Row,Col}  VectorPEs inside ONE OuterPE
  //   multPerPe               multiplier lanes inside ONE VectorPE
  //
  // A conventional one-MAC-per-cell systolic array is just the degenerate case
  // localVectorPe* == multPerPe == 1.
  def outerPeRow: Int
  def outerPeCol: Int
  def localVectorPeRow: Int
  def localVectorPeCol: Int
  def multPerPe: Int

  /** Whether an external `AccSram` accumulator is attached after the array. Never true
    * for ReuseC, which folds the contraction into the PE — enforced below. */
  def needAccumulator: Boolean

  // ── 2. Derived geometry ─────────────────────────────────────────────────────────
  // Three different UNITS are in play here and confusing them is the usual source of a
  // width bug, so each has its own name:
  //
  //   outerPe*              OuterPEs across the fabric
  //   totalVectorPe*        VectorPEs across the fabric   (= outerPe* x localVectorPe*)
  //   systolicArray*Dimension   MULTIPLIER LANES across it    (= totalVectorPe* x multPerPe)
  //
  // The last is the one that matters arithmetically: it is the operand vector the
  // fabric consumes per cycle on that axis, and so the unit the contraction depth and
  // every accumulation width are counted in.

  /** Multiplier lanes one OuterPE presents on its row / column edge. */
  lazy val outerPeRowDimension: Int = localVectorPeRow * multPerPe
  lazy val outerPeColDimension: Int = localVectorPeCol * multPerPe

  /** VectorPE count down each axis — the PE grid as the topology sees it. */
  lazy val totalVectorPeRow: Int = outerPeRow * localVectorPeRow
  lazy val totalVectorPeCol: Int = outerPeCol * localVectorPeCol

  /** Multiplier lanes across the whole fabric on each axis. Equivalently
    * `totalVectorPe* * multPerPe`; `PortTypeProvider.systolicOutputBitwidth` counts
    * exactly these products when it sizes the array's C port. */
  lazy val systolicArrayRowDimension: Int = outerPeRow * outerPeRowDimension
  lazy val systolicArrayColDimension: Int = outerPeCol * outerPeColDimension

  // ── 1b. Declared, but CONDITIONALLY ────────────────────────────────────────────
  // The two sizing fields. Unlike everything in section 1 these are not universal: a
  // config declares the one its own hardware consumes and leaves the other at 0, and
  // section 3 rejects it for getting that wrong in either direction. They sit here,
  // after the geometry, because their doc is written in terms of it.

  /**
   * Extent of the FREE (non-contracted) output dimension the accumulator holds —
   * N for ReuseA, M for ReuseB. Equals the [[AccSram]] depth: one address per
   * accumulator slot, so a whole panel of output elements stays resident while
   * partial sums are summed across K.
   *
   * Declared ONLY by an accumulator ReuseA/ReuseB config (0 otherwise). It is a
   * provisioning cap, not the GEMM's own extent: a GEMM whose N (resp. M) is
   * smaller runs fine and simply leaves slots unused — `GoldenMatrix` clamps the
   * tile with `min(cap, gemmDim.N)`.
   */
  def freeDimensionSize: Int

  /**
   * Total contraction (K) extent this array is PROVISIONED for, i.e. the largest
   * GEMM K it may be driven with. Like `freeDimensionSize` it is a cap: a smaller
   * GEMM K runs fine.
   *
   * What it sizes depends on where the contraction lands:
   *
   *  - ReuseA / ReuseB (accumulator): the EXTERNAL accumulator's data width, which
   *    grows with the number of partial sums summed per output element
   *    (`contractionPasses`). This is a second, independent axis from
   *    `freeDimensionSize` — that one is the accumulator DEPTH, an address-space
   *    quantity — which is why these two dataflows are the only ones declaring both.
   *  - ReuseC on Grouped: the PE's OWN output width AND the K a golden tile spans,
   *    since the contraction is folded inside the PE. The K a PE accumulates is the
   *    number of cycles `accEnableC` is held high, which nothing in the array
   *    dimensions divides, so it is a usage parameter — REQUIRED, with no
   *    architectural bound above or below it. Raising it makes the drivers accumulate
   *    that much in one burst.
   *  - ReuseC on Toroidal: NOT declarable. The wrap recirculates the operands, so one
   *    burst accumulates exactly the cluster-loop length and the dimensions fix the
   *    width architecturally.
   */
  def maxContractionSize: Int

  // ── 2b. Derived contraction geometry ────────────────────────────────────────────
  // Where the K of a GEMM actually goes. This is the one derivation the three dataflows
  // genuinely disagree about, which is why it is a match and not a formula.

  /**
   * K products consumed by ONE pass through the fabric (one k-tile), for the dataflows
   * where the fabric bounds K at all. Matches the `tileK` GoldenMatrix uses and the
   * spatial reduction `systolicOutputBitwidth` already accounts for.
   *
   * UNDEFINED for Grouped ReuseC — see below.
   */
  lazy val contractionPassDepth: Int = dataflow match {
    // Contraction is SPATIAL: summed down the array's adder chain.
    case Dataflow.ReuseA => systolicArrayColDimension
    case Dataflow.ReuseB => systolicArrayRowDimension
    case Dataflow.ReuseC =>
      // Grouped ReuseC has no such quantity. Its K is a CYCLE COUNT (how long
      // `accEnableC` is held) and no fabric dimension bounds cycles, so the only bound
      // is the PE accumulator width — `maxContractionSize`. Returning a plausible
      // number here is exactly how an array-width bound ended up in GoldenMatrix's
      // ReuseC tileK, so fail loudly rather than hand one out.
      require(isToroidal,
        "[error] contractionPassDepth is undefined for Grouped ReuseC: its contraction is temporal, " +
          "bounded only by maxContractionSize (the PE accumulator width), not by any array dimension")
      // Toroidal: the wrap recirculates the operands, so one burst carries the
      // CLUSTER-loop length (outerPeRow hops × multPerPe lanes) — NOT the physical row
      // count, which broadcast (lvR/lvC > 1) inflates without adding any K depth.
      // Verified: forcing a tile past this length fails all three broadcast suites.
      outerPeRow * multPerPe
  }

  /**
   * Partial sums the external accumulator adds per output element = ceil(K / tileK).
   *
   * The second half of the accumulator's width story: `contractionPassDepth` is how
   * much K one fabric pass reduces SPATIALLY, this is how many such passes the
   * accumulator then sums TEMPORALLY, and `PortTypeProvider.accumulatorOutputBitWidth`
   * adds `log2Up` of it on top of the array's own output width.
   *
   * `max(1, ...)` so a config that declares no K at all still reports one pass rather
   * than zero; the ceiling itself is the integer-division idiom `(a + b - 1) / b`.
   */
  lazy val contractionPasses: Int =
    math.max(1, (maxContractionSize + contractionPassDepth - 1) / contractionPassDepth)

  // ── 3. Validation ───────────────────────────────────────────────────────────────
  // Statements in the trait body, so they run at CONSTRUCTION — a config object cannot
  // exist in an invalid state, and no elaboration is ever reached with one. (The
  // subclasses' constructor parameters are already assigned by the time this runs,
  // which is what lets the trait check fields it only declares abstractly.)
  //
  // First the dataflow-independent floors.
  require(outerPeRow >= 1, s"[error]Outer PE Row must be non-negative, got $outerPeRow")
  require(outerPeCol >= 1, s"[error]Outer PE Col must be non-negative, got $outerPeCol")
  require(localVectorPeRow >= 1, s"[error]Local Local Vector PE Row must be non-negative, got $localVectorPeRow")
  require(localVectorPeCol >= 1, s"[error]Local Local Vector Col must be non-negative, got $localVectorPeCol")
  // A VectorPE reduces its own `multPerPe` products with a BALANCED adder tree, which
  // only exists for a power-of-two leaf count.
  require(isPow2(multPerPe), "[error] Number of multipliers per PE should be power of 2")
  // Counted in multiplier LANES, not PEs: a 1x1 fabric with multPerPe = 2 passes. Below
  // two lanes on an axis there is no systolic movement left to model.
  require(systolicArrayRowDimension >= 2, "[error] Systolic array at least 2 total vector PE row")
  require(systolicArrayColDimension >= 2, "[error] Systolic array at least 2 total vector PE col")
  require(bitWidthA >= 1, s"[error] Bit Width must be non-negative $bitWidthA")
  require(bitWidthB >= 1, s"[error] Bit Width must be non-negative $bitWidthB")
  // Then the per-dataflow ownership of the two sizing fields. Read the three blocks
  // below as one table: for each (dataflow, accumulator) combination, exactly which of
  // `freeDimensionSize` / `maxContractionSize` must be present and which must be 0.
  if(needAccumulator){
    require(dataflow != Dataflow.ReuseC, "[error] Reuse C (Output Stationary) dose not need accumulator")
    // AccSram's pipelined synchronous read-modify-write needs a slot to be revisited
    // no sooner than the cycle after its write commits (see AccSram).
    require(freeDimensionSize >= 2,
      s"[error] An accumulator config must declare freeDimensionSize >= 2 (the AccSram depth), got $freeDimensionSize")
    require(maxContractionSize >= 1,
      s"[error] An accumulator config must declare maxContractionSize (the largest GEMM K it is provisioned for), got $maxContractionSize")
  }

  // Dimension fields are owned per dataflow: a config declares exactly the extents its
  // own hardware consumes, so `freeDimensionSize` and `maxContractionSize` never
  // coexist outside the one case that genuinely needs two independent axes
  // (ReuseA/ReuseB + accumulator: depth AND data width).
  if (isReuseC) {
    require(freeDimensionSize == 0,
      s"[error] ReuseC is output-stationary and has no AccSram, so it must not declare freeDimensionSize, got $freeDimensionSize")
    if (isGrouped) {
      // Required: the K a Grouped ReuseC PE accumulates is a usage parameter (how long
      // `accEnableC` is held), so leaving it out would declare an output width that sums
      // nothing. There is no architectural upper OR lower bound to check it against —
      // K is a cycle count, and no fabric dimension bounds cycles — so this value alone
      // is what sizes the PE output and what GoldenMatrix cuts ReuseC tiles with.
      require(maxContractionSize >= 1,
        s"[error] Grouped ReuseC folds the contraction inside the PE, so it must declare maxContractionSize " +
          s"(the K one PE accumulates), got $maxContractionSize")
      require(maxContractionSize % multPerPe == 0,
        s"[error] Grouped ReuseC streams multPerPe ($multPerPe) K values per cycle, so maxContractionSize " +
          s"must be a multiple of it, got $maxContractionSize")
    }
    else
      require(maxContractionSize == 0,
        s"[error] Toroidal ReuseC accumulates the wrap's cluster-loop length ($contractionPassDepth) per burst, " +
          s"which the array dimensions fix; it must not declare maxContractionSize, got $maxContractionSize")
  }
  if (isReuseAB && !needAccumulator) {
    require(freeDimensionSize == 0 && maxContractionSize == 0,
      s"[error] A ReuseA/ReuseB config without an accumulator sizes its output from the array dimensions alone; " +
        s"it must declare neither freeDimensionSize (got $freeDimensionSize) nor maxContractionSize (got $maxContractionSize)")
  }

  // ── 4. Derived views ────────────────────────────────────────────────────────────

  /**
   * The wiring plan this config implies — which neighbour each PE edge connects to,
   * and where operands enter and leave.
   *
   * `lazy` is load-bearing: `ArrayTopology(this)` reads the config straight back, so
   * building it eagerly in the trait body would hand out a half-constructed `this`.
   */
  lazy val topology: ArrayTopology = ArrayTopology(this)

  /** Short alias for [[dataflow]], for call sites that match on it repeatedly. */
  final def df: Dataflow.Value = this.dataflow

  // Predicates. `final` throughout: these are derivations of the fields above, never
  // something an individual config may redefine to mean something else.
  final def isReuseA: Boolean = dataflow == Dataflow.ReuseA
  final def isReuseB: Boolean = dataflow == Dataflow.ReuseB
  final def isReuseC: Boolean = dataflow == Dataflow.ReuseC
  final def isReuseAB: Boolean = (dataflow == Dataflow.ReuseA) || (dataflow == Dataflow.ReuseB)

  // Fabric tests, by CLASS rather than by a field — which is why the trait is sealed.
  final def isGrouped: Boolean = this.isInstanceOf[GroupedConfig]
  final def isToroidal: Boolean = this.isInstanceOf[ToroidalConfig]

  /**
   * True for the dual-mode wrapper config.
   *
   * `ToroidalDualModeConfig` extends [[SystolicArrayConfig]] DIRECTLY, not
   * [[ToroidalConfig]], so `isToroidal` is deliberately FALSE for it — every existing
   * `isToroidal` dispatch keeps its exact present meaning and the stock runners are
   * never silently handed a dual config. Use [[fabric]] to dispatch across all three.
   */
  final def isDualMode: Boolean = this.isInstanceOf[ToroidalDualModeConfig]

  /** The fabric this config elaborates — the first axis of the harness dispatch key. */
  final def fabric: Fabric.Value =
    if (isDualMode)     Fabric.ToroidalDual
    else if (isGrouped) Fabric.Grouped
    else                Fabric.Toroidal

  /**
   * A 1x1 cluster: the OuterPE wrapper degenerates to a bare VectorPE, so no GroupPE /
   * ClusterPE is elaborated at all and the inner chain disappears with it. Several
   * components take a visibly simpler path in this case, which is why it has a name.
   */
  final def isStandAloneVectorPeConfig: Boolean =
    localVectorPeRow == 1 && localVectorPeCol == 1

  /** The complements: a fabric whose OuterPEs really are composite, per fabric. */
  final def isGroupPeConfig: Boolean =
    isGrouped && !isStandAloneVectorPeConfig

  final def isClusterPeConfig: Boolean =
    isToroidal && !isStandAloneVectorPeConfig

  // ── 5. Guards ───────────────────────────────────────────────────────────────────
  // Called from code that is only meaningful for one fabric or one dataflow. They turn
  // "this component was handed a config it cannot build from" into an elaboration-time
  // failure with a named reason, instead of hardware that is wired but meaningless.
  final def requireGrouped(): Unit =
    require(isGrouped, "[error] This operation is only valid for Grouped Systolic Array")

  final def requireToroidal(): Unit =
    require(isToroidal, "[error] This operation is only valid for Toroidal Systolic Array")

  final def requireStandAlone(): Unit =
    require(isStandAloneVectorPeConfig,
      "[error] This operation requires standalone VectorPE config (1x1)")

  final def requireReuseA(): Unit =
    require(isReuseA, "[error] This operation is only valid for ReuseA dataflow")

  final def requireReuseB(): Unit =
    require(isReuseB, "[error] This operation is only valid for ReuseB dataflow")

  final def requireReuseC(): Unit =
    require(isReuseC, "[error] This operation is only valid for ReuseC dataflow")

  // Narrowing casts, for the few call sites that genuinely need the subclass rather
  // than the trait. Throwing with a reason beats `asInstanceOf`, whose ClassCastException
  // would name the classes and nothing about why the component wanted one.
  final def asGrouped: GroupedConfig = this match {
    case grouped: GroupedConfig => grouped
    case _ => throw new IllegalStateException("Only valid in Grouped Systolic Array")
  }

  final def asToroidal: ToroidalConfig = this match {
    case toroidal: ToroidalConfig => toroidal
    case _ => throw new IllegalStateException("Only valid in Toroidal Systolic Array")
  }

}

/**
 * A GROUPED array: OuterPEs are GroupPEs on a plain mesh. Operands enter at the edges,
 * partial sums walk the dataflow's axis and leave at the far edge, and nothing comes
 * back — so how much K one pass reduces is fixed by how many lanes that axis has.
 *
 * The two requires are one constraint stated per dataflow. The VectorPEs inside a
 * GroupPE are reduced by a balanced adder tree, so the count on the REDUCTION axis must
 * be a power of two — columns for ReuseA, rows for ReuseB. The other axis is left free,
 * which is what lets a group be rectangular.
 */
case class GroupedConfig(
  numericType: NumericType,
  dataflow: Dataflow.Value,
  outerPeRow: Int,
  outerPeCol: Int,
  localVectorPeRow:Int,
  localVectorPeCol: Int,
  multPerPe: Int,
  bitWidthA: Int,
  bitWidthB: Int,
  needAccumulator: Boolean,
  freeDimensionSize: Int = 0,
  maxContractionSize: Int = 0,
) extends SystolicArrayConfig {

  if(isReuseA) require(isPow2(localVectorPeCol),
    s"[error] Grouped ReuseA Local Vector PE Col decides partial sum adder tree dimension, it muse be power of 2, got $localVectorPeCol")

  if(isReuseB) require(isPow2(localVectorPeRow),
    s"[error] Grouped ReuseB Local Vector PE Row decides partial sum adder tree dimension, it muse be power of 2, got $localVectorPeRow")

}

/**
 * A TOROIDAL array: the edges wrap, so operands recirculate instead of being re-fed
 * from the buffers. That is what makes one burst's contraction a property of the loop
 * length rather than of a driver's tiling — see [[SystolicArrayConfig.contractionPassDepth]].
 *
 * On top of the Grouped power-of-two rule, the wrap forces the two axes to MATCH: a
 * value leaving the last column has to arrive back at the first row on the cycle the
 * schedule expects it, which is `outerPeCol * localVectorPeCol == outerPeRow` for
 * ReuseA and the mirror for ReuseB. ReuseC needs only the plain square `outerPeRow ==
 * outerPeCol`, since with the contraction folded into the PE there is no reduction axis
 * to fold into the other one.
 *
 * Several STRICTER requires are commented out below and are NOT enforced — a config has
 * only to satisfy the live ones. They would have pinned the sub-array dimensions to 1
 * and the two totals to each other, i.e. forbidden exactly the broadcast shapes
 * (`localVectorPe* > 1`) the live constraints now admit.
 */
case class ToroidalConfig(
  numericType: NumericType,
  dataflow: Dataflow.Value,
  outerPeRow: Int,
  outerPeCol: Int,
  localVectorPeRow:Int,
  localVectorPeCol: Int,
  multPerPe: Int,
  bitWidthA: Int,
  bitWidthB: Int,
  needAccumulator: Boolean,
  freeDimensionSize: Int = 0,
  maxContractionSize: Int = 0,
) extends SystolicArrayConfig {

  // require(totalVectorPeRow == totalVectorPeCol,
    // s"[error] Total vector PE Row and total vector PE Column must be same")

  if(isReuseA) {
    require(isPow2(localVectorPeCol),
      s"[error] Toroidal ReuseA Local Vector PE Col decides partial sum adder tree dimension, it muse be power of 2, got $localVectorPeCol")
    // require(localVectorPeRow == 1,
      // s"[error] Toroidal ReuseA Local Vector PE Row must be 1, got $localVectorPeRow")
      require(outerPeCol * localVectorPeCol == outerPeRow, "[error] Toroidal ReuseA outer PE row must be multiplication of outer PE col and local vector PE col")
  }

  if(isReuseB){
    require(isPow2(localVectorPeRow),
      s"[error] Toroidal ReuseB Local Vector PE Col decides partial sum adder tree dimension, it muse be power of 2, got $localVectorPeRow")
    // require(localVectorPeCol == 1,
      // s"[error] Toroidal ReuseA Local Vector PE Row must be 1, got $localVectorPeCol")
      require(outerPeRow * localVectorPeRow == outerPeCol, "[error] Toroidal ReuseB outer PE col must be multiplication of outer PE row and local vector PE row")
  }

  if(isReuseC) {
    // require(localVectorPeRow == 1 && localVectorPeCol == 1,
      // "[error] ReuseC Local Vector PE Row and Local Vector PE Col must be 1")
    require(outerPeRow == outerPeCol, "[error] Toroidal ReuseC only allows square systolic arrays")
  }

  // private val numOuterDiagonals: Int = localVectorPeRow + localVectorPeCol - 1
  // val clusterPeDiagonalInputDim: Int = numOuterDiagonals * multPerPe

}

/**
 * A Toroidal array with a SECOND, time-shared input stream: one elaborated fabric runs
 * both a tiled GEMM and a low-M GEMV, switching between them with no re-elaboration and
 * no reset. `StationaryDemux` and the extra entry stages are what the mode select
 * drives; the fabric inside is stock Toroidal.
 *
 * ReuseC is rejected outright. A mode switch re-drives the same PEs with a different
 * operand routing, and an output-stationary PE is holding the result it is accumulating
 * across exactly that boundary.
 *
 * It extends [[SystolicArrayConfig]] DIRECTLY rather than [[ToroidalConfig]] — see
 * [[SystolicArrayConfig.isDualMode]] for why — and that is why the dimension requires
 * below are duplicated from `ToroidalConfig` instead of inherited. Anything that wants
 * the plain fabric asks [[toToroidalConfig]] for it.
 */
case class ToroidalDualModeConfig(
  numericType: NumericType,
  dataflow: Dataflow.Value,
  outerPeRow: Int,
  outerPeCol: Int,
  localVectorPeRow:Int,
  localVectorPeCol: Int,
  multPerPe: Int,
  bitWidthA: Int,
  bitWidthB: Int,
  needAccumulator: Boolean,
  freeDimensionSize: Int = 0,
  maxContractionSize: Int = 0,
) extends SystolicArrayConfig {

  // Dead: an earlier revision pinned this config to a single dataflow. Both are now
  // supported and the live constraint is the ReuseC rejection below.
  // require(dataflow == Dataflow.ReuseA, "[error]")
  // require(dataflow == Dataflow.ReuseB, "[error]")

  if(isReuseA) {
    require(isPow2(localVectorPeCol),
      s"[error] Toroidal ReuseA Local Vector PE Col decides partial sum adder tree dimension, it muse be power of 2, got $localVectorPeCol")
    // require(localVectorPeRow == 1,
      // s"[error] Toroidal ReuseA Local Vector PE Row must be 1, got $localVectorPeRow")
      require(outerPeCol * localVectorPeCol == outerPeRow, "[error] Toroidal ReuseA outer PE row must be multiplication of outer PE col and local vector PE col")
  }

  if(isReuseB){
    require(isPow2(localVectorPeRow),
      s"[error] Toroidal ReuseB Local Vector PE Col decides partial sum adder tree dimension, it muse be power of 2, got $localVectorPeRow")
    // require(localVectorPeCol == 1,
      // s"[error] Toroidal ReuseA Local Vector PE Row must be 1, got $localVectorPeCol")
      require(outerPeRow * localVectorPeRow == outerPeCol, "[error] Toroidal ReuseB outer PE col must be multiplication of outer PE row and local vector PE row")
  }

  require(!isReuseC, "[error] Dual mode does not allow ReuseC (output stationary)")

  /**
   * The plain Toroidal config this dual-mode array's FABRIC is built from — same
   * dimensions, minus the dual-mode marking.
   *
   * Dual mode adds ports and entry stages around a stock Toroidal fabric, nothing
   * inside it, so everything dispatched on the config's CLASS is dispatched on this
   * one: [[ArrayTopology.apply]], `SystolicArray.buildDefinitionName`,
   * `attachAccumulatorIfNeeded` and [[GoldenMatrix]]'s normal-GEMM tiling all have a
   * `ToroidalConfig` case and no `ToroidalDualModeConfig` one. The array itself is
   * built as `new ToroidalSystolicArray(cfg.toToroidalConfig, dualMode = true)`.
   */
  def toToroidalConfig: ToroidalConfig = ToroidalConfig(
    numericType        = numericType,
    dataflow           = dataflow,
    outerPeRow         = outerPeRow,
    outerPeCol         = outerPeCol,
    localVectorPeRow   = localVectorPeRow,
    localVectorPeCol   = localVectorPeCol,
    multPerPe          = multPerPe,
    bitWidthA          = bitWidthA,
    bitWidthB          = bitWidthB,
    needAccumulator    = needAccumulator,
    freeDimensionSize  = freeDimensionSize,
    maxContractionSize = maxContractionSize,
  )

  // Dead code from earlier revisions, kept only as a record of what was relaxed; none
  // of it is enforced.
  // override val dataflow: Dataflow.Value = Dataflow.ReuseB

  // require(totalVectorPeRow == totalVectorPeCol,
    // s"[error] Total vector PE Row and total vector PE Column must be same")

  // if(isReuseB){
  //   require(isPow2(localVectorPeRow),
  //     s"[error] Toroidal ReuseB Local Vector PE Col decides partial sum adder tree dimension, it muse be power of 2, got $localVectorPeRow")
  //   // require(localVectorPeCol == 1,
  //     // s"[error] Toroidal ReuseA Local Vector PE Row must be 1, got $localVectorPeCol")
  //     require(outerPeRow * localVectorPeRow == outerPeCol, "[error] Toroidal ReuseB outer PE col must be multiplication of outer PE row and local vector PE row")
  // }

  // private val numOuterDiagonals: Int = localVectorPeRow + localVectorPeCol - 1
//   val clusterPeDiagonalInputDim: Int = numOuterDiagonals * multPerPe

}



