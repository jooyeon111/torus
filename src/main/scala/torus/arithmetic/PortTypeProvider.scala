package torus.arithmetic

import spinal.core._

import torus.config._
import torus.core._

/**
 * Every hardware type and typed zero used anywhere in the fabric, derived from one
 * [[SystolicArrayConfig]].
 *
 * [[Arithmetic]] supplies the OPERATIONS; this supplies the WIDTHS. Nothing in the
 * design declares a bit width of its own — a PE, an adder tree or an accumulator asks
 * this provider for the type of the port it is driving, so a width can only ever be
 * wrong in one place.
 *
 * The rule behind every formula here: an exact product of a `bitWidthA` and a
 * `bitWidthB` value needs `bitWidthA + bitWidthB` bits, and summing `n` such products
 * without overflow needs `log2Up(n)` more. So every width below is
 * `multOutputBitWidth + log2Up(products summed so far)` — which makes the widths GROW
 * ALONG THE PARTIAL-SUM CHAIN rather than being uniform. That is deliberate: an
 * early-chain port that can only ever hold a few products is not paid for as if it
 * held the whole reduction. It is also why the per-PE methods take the PE's position
 * ([[OuterPeIndex]], [[InnerIndex]]) — the width is a function of where in the chain
 * the port sits.
 *
 * WHICH axis the chain runs along mirrors with the dataflow, and the three dataflows
 * are genuinely different shapes:
 * {{{
 *   ReuseA   partial sums walk COLUMNS  -> width grows with (c * localVectorPeCol + b)
 *   ReuseB   partial sums walk ROWS     -> width grows with (r * localVectorPeRow + a)
 *   ReuseC   contraction folded in the PE -> no chain, every C port is one flat width
 * }}}
 *
 * Implemented once per numeric type ([[SignedPortTypeProvider]] /
 * [[UnsignedPortTypeProvider]]); the formulas are identical and only `SInt` vs `UInt`
 * differs. Components take it implicitly alongside the matching [[Arithmetic]].
 */
trait PortTypeProvider [InputType <: Data, AccType <: Data] {

  val config: SystolicArrayConfig

  // Exact product width — the baseline every accumulation width below is built on.
  // Never truncated anywhere in the design, so a MAC result is always bit-exact.
  final protected val multOutputBitWidth: Int = config.bitWidthA + config.bitWidthB

  /**
   * Width of the array's C output port — the result of ONE pass through the fabric,
   * before any external accumulator. For ReuseA / ReuseB that is the full spatial
   * reduction down the partial-sum chain (`multPerPe` lanes x every PE on the axis);
   * for ReuseC the contraction never leaves the PE, so the three cases below are not
   * variations on one formula but three different questions about where K lives.
   */
  final protected val systolicOutputBitwidth: Int = config.dataflow match {
    // Products summed along a full COLUMN chain: multPerPe lanes x every VectorPE
    // column (outerPeCol clusters x localVectorPeCol sub-columns each).
    case Dataflow.ReuseA =>
      multOutputBitWidth + log2Up(config.multPerPe * config.outerPeCol * config.localVectorPeCol)
    // Mirror of ReuseA along the ROW axis.
    case Dataflow.ReuseB =>
      multOutputBitWidth + log2Up(config.multPerPe * config.outerPeRow * config.localVectorPeRow)
    case Dataflow.ReuseC =>
      // Output-stationary: the contraction is folded inside the PE, so the growth over
      // one product is log2 of the K that PE accumulates. WHERE that K comes from
      // differs by architecture.
      //
      // Grouped: K is the number of cycles `accEnableC` is held high (VectorPE feeds
      // partialSumReg back until the falling edge drains it), which nothing in the
      // array dimensions divides — a usage parameter, so the config must declare it
      // via `maxContractionSize` (SystolicArrayConfig requires it, at least one pass
      // through the fabric). No floor is applied here: an undeclared K would be a
      // config error, not something to paper over with a default width.
      //
      // Toroidal: the wrap recirculates the operands, so one burst accumulates exactly
      // the cluster-loop length and the dimensions DO fix the width. Such configs may
      // not declare a K at all (also enforced), so nothing is consulted here.
      if (config.isGrouped)
        multOutputBitWidth + log2Up(config.maxContractionSize)
      else
        multOutputBitWidth + log2Up(config.contractionPassDepth)
  }

  // The accumulator sums one systolic output per K-pass, so its growth over
  // `systolicOutputBitwidth` is log2 of the NUMBER OF PASSES, ceil(K / tileK).
  // (It is NOT log2 of the bank depth `freeDimensionSize` — that is an
  // address-space quantity and only coincidentally large enough.)
  final protected def accumulatorOutputBitWidth: Int = {
    config.dataflow match {
      case Dataflow.ReuseA | Dataflow.ReuseB =>
        systolicOutputBitwidth + log2Up(config.contractionPasses)
      case Dataflow.ReuseC =>
        throw new IllegalArgumentException(
          "ReuseC (Output Stationary) dataflow does not support external accumulation. " +
            "Partial sums are accumulated internally within each PE."
        )
    }
  }

  // ── Hardware types, implemented per numeric type ────────────────────────────────
  // Abstract rather than final because ONLY the SInt/UInt choice differs; the widths
  // themselves are fixed by the formulas above and repeated identically in both
  // implementations.

  def createInputTypeA: InputType
  def createInputTypeB: InputType
  def createMultOutputType: AccType

  /**
   * Output of ONE VectorPE's internal multiplier tree, before any chain input is added:
   * its own `multPerPe` products and nothing else. Position-independent, so unlike the
   * C-port types below it needs no index.
   */
  final def createVectorPeDotProductType: AccType =
    createAccumulationType(multOutputBitWidth + log2Up(config.multPerPe))

  protected def createAccumulationType(width: Int): AccType

  // The C-port types, from the innermost port outwards. Each takes the port's POSITION
  // because the width is a function of how much of the chain is already behind it:
  //
  //   VectorPe{Input,Output}TypeC   one sub-row/sub-column step inside a cluster
  //   OuterPe{Input,Output}TypeC    one whole cluster step (GroupPE / ClusterPE)
  //   SystolicOutputTypeC           the array's port: the entire chain, one fabric pass
  //   AccumulatorOutputTypeC        after the external accumulator sums the K passes
  //
  // Input vs Output of the same PE differ by exactly that PE's own contribution, so an
  // `outputC` is at most one bit wider than the matching `inputC`.
  def createVectorPeInputTypeC(outerIndex: OuterPeIndex, innerIndex: InnerIndex): AccType
  def createVectorPeOutputTypeC(outerIndex: OuterPeIndex, innerIndex: InnerIndex): AccType
  def createOuterPeInputTypeC(outerIndex: OuterPeIndex): AccType
  def createOuterPeOutputTypeC(outerIndex: OuterPeIndex): AccType
  def createSystolicOutputTypeC: AccType
  def createAccumulatorOutputTypeC: AccType

  // ── Typed zeros ─────────────────────────────────────────────────────────────────
  // One `zeroX` per `createX`, each just `createX`'s width handed to [[Arithmetic]].
  // They exist as their own family because a `Reg`/`Delay` init must match its port's
  // width EXACTLY — deriving the zero from the same method that declared the port is
  // what stops an init and its register from drifting apart.

  final def zeroInputA(implicit arithmetic: Arithmetic[InputType, AccType]): InputType =
    arithmetic.zeroInput(createInputTypeA.getBitsWidth)

  final def zeroInputB(implicit arithmetic: Arithmetic[InputType, AccType]): InputType =
    arithmetic.zeroInput(createInputTypeB.getBitsWidth)

  final def zeroMultOutput(implicit arithmetic: Arithmetic[InputType, AccType]): AccType =
    arithmetic.zeroAccumulation(createMultOutputType.getBitsWidth)

  final def zeroVectorPeInputTypeC(outerIndex: OuterPeIndex, innerIndex: InnerIndex)(
    implicit arithmetic: Arithmetic[InputType, AccType]
  ): AccType  =
    arithmetic.zeroAccumulation(createVectorPeInputTypeC(outerIndex, innerIndex).getBitsWidth)

  final def zeroVectorPeOutputTypeC(outerIndex: OuterPeIndex, innerIndex: InnerIndex)(
    implicit arithmetic: Arithmetic[InputType, AccType]
  ): AccType  =
    arithmetic.zeroAccumulation(createVectorPeOutputTypeC(outerIndex, innerIndex).getBitsWidth)

  final def zeroOuterPeInputTypeC(groupIndex: OuterPeIndex)(
    implicit arithmetic: Arithmetic[InputType, AccType]
  ): AccType =
    arithmetic.zeroAccumulation(createOuterPeInputTypeC(groupIndex).getBitsWidth)

  final def zeroOuterPeOutputTypeC(groupIndex: OuterPeIndex)(
    implicit arithmetic: Arithmetic[InputType, AccType]
  ): AccType =
    arithmetic.zeroAccumulation(createOuterPeOutputTypeC(groupIndex).getBitsWidth)

  final def zeroVectorPeDotProduct(
    implicit arithmetic: Arithmetic[InputType, AccType]
  ): AccType =
    arithmetic.zeroAccumulation(createVectorPeDotProductType.getBitsWidth)

  final def zeroSystolicOutputTypeC (
    implicit arithmetic: Arithmetic[InputType, AccType]
  ): AccType =
    arithmetic.zeroAccumulation(createSystolicOutputTypeC.getBitsWidth)

  final def zeroAccumulatorOutputTypeC(
    implicit arithmetic: Arithmetic[InputType, AccType]
  ): AccType =
    arithmetic.zeroAccumulation(createAccumulatorOutputTypeC.getBitsWidth)

  // ── Tile-keyed lookups ──────────────────────────────────────────────────────────
  // For the buffers, which are written once and instantiated per operand: a
  // [[SkewBuffer]] or [[RegisterBuffer]] is told only WHICH tile it carries and asks
  // for the type by that key, instead of every call site branching on A vs B. The
  // TypeC cases throw rather than return something plausible — an operand buffer
  // handed TileType.C is a wiring mistake, and a silently-returned width would only
  // surface much later as a mismatched port.

  final def createInputTypeByTile(tileType: TileType.Value): InputType = tileType match {
    case TileType.TypeA => createInputTypeA
    case TileType.TypeB => createInputTypeB
    case TileType.TypeC =>
      throw new IllegalArgumentException(
        s"[error] No input type defined for tileType: $tileType")
  }

  final def zeroInputByTile(tileType: TileType.Value)(
    implicit arithmetic: Arithmetic[InputType, AccType]
  ): InputType = tileType match {
    case TileType.TypeA => zeroInputA
    case TileType.TypeB => zeroInputB
    case TileType.TypeC =>
      throw new IllegalArgumentException(
        s"[error] No zero input defined for tileType: $tileType")
  }

  final def createAccTypeByTile(tileType: TileType.Value): AccType = tileType match {
    case TileType.TypeC => createSystolicOutputTypeC
    case other =>
      throw new IllegalArgumentException(
        s"[error] No acc type defined for tileType: $other")
  }

  final def zeroAccTypeByTile(tileType: TileType.Value)(
    implicit arithmetic: Arithmetic[InputType, AccType]
  ): AccType = tileType match {
    case TileType.TypeC => zeroSystolicOutputTypeC
    case other =>
      throw new IllegalArgumentException(
        s"[error] No zero acc defined for tileType: $other")
  }

}

/**
 * [[PortTypeProvider]] for a `NumericType.Signed` config: every port is an `SInt`.
 *
 * The four C-port methods below are where the trait's "width grows along the chain"
 * rule becomes arithmetic, so read them as one family. Writing ReuseB's version (ReuseA
 * is its mirror, r/a/localVectorPeRow for c/b/localVectorPeCol), the chain position of
 * a VectorPE is its GLOBAL sub-row index `k = r * localVectorPeRow + a`, and every
 * width is `multOutputBitWidth + log2Up(multPerPe * <products already summed>)`:
 * {{{
 *   VectorPe  inputC   k          products upstream       (k=0: nothing upstream)
 *   VectorPe  outputC  k + 1      upstream + its own lane
 *   OuterPe   inputC   r*lvR      the clusters above it
 *   OuterPe   outputC  r*lvR+lvR  those plus all lvR of its own sub-rows
 * }}}
 * Two details that look like noise and are not:
 *
 *  - `.max(1)` on the INPUT widths only. The first PE on a chain has zero products
 *    upstream and `log2Up(0)` is not a width; clamping to 1 gives it a plain
 *    `multOutputBitWidth` port. The OUTPUT widths never need it — a PE always
 *    contributes its own lane, so the count is at least 1 already.
 *  - ReuseC returns the flat `systolicOutputBitwidth` for ALL four. Output-stationary
 *    keeps the contraction inside the PE, so no C port accumulates along a chain and
 *    there is nothing for the position to vary.
 */
class SignedPortTypeProvider(val config: SystolicArrayConfig) extends PortTypeProvider[SInt, SInt]{

  override def createInputTypeA: SInt = SInt(config.bitWidthA bits)
  override def createInputTypeB: SInt = SInt(config.bitWidthB bits)
  override def createMultOutputType: SInt = SInt(multOutputBitWidth bits)
  override protected def createAccumulationType(width: Int): SInt = SInt(width bits)

  override def createVectorPeInputTypeC(outerIndex: OuterPeIndex, innerIndex: InnerIndex): SInt = {

    config.dataflow match {
      case Dataflow.ReuseA =>
        SInt((multOutputBitWidth + log2Up((config.multPerPe * (outerIndex.c * config.localVectorPeCol + innerIndex.b)).max(1))) bits)
      case Dataflow.ReuseB =>
        SInt((multOutputBitWidth + log2Up((config.multPerPe * (outerIndex.r * config.localVectorPeRow + innerIndex.a)).max(1))) bits)
      case Dataflow.ReuseC =>
        SInt(systolicOutputBitwidth bits)

    }

  }

  override def createVectorPeOutputTypeC(outerIndex: OuterPeIndex, innerIndex: InnerIndex): SInt = {

    config.dataflow match {
      case Dataflow.ReuseA =>
        SInt( (multOutputBitWidth + log2Up(config.multPerPe * (outerIndex.c * config.localVectorPeCol + innerIndex.b + 1))) bits)

      case Dataflow.ReuseB =>
        SInt( (multOutputBitWidth + log2Up(config.multPerPe * (outerIndex.r * config.localVectorPeRow + innerIndex.a + 1))) bits)

      case Dataflow.ReuseC =>
        SInt( systolicOutputBitwidth bits )

    }
  }

  override def createOuterPeInputTypeC(outerIndex: OuterPeIndex): SInt = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        SInt( (multOutputBitWidth + log2Up((config.multPerPe * outerIndex.c * config.localVectorPeCol).max(1))) bits)

      case Dataflow.ReuseB =>
        SInt( (multOutputBitWidth + log2Up((config.multPerPe * outerIndex.r * config.localVectorPeRow).max(1))) bits)

      case Dataflow.ReuseC =>
        SInt( systolicOutputBitwidth bits )

    }
  }

  def createOuterPeOutputTypeC(outerIndex: OuterPeIndex): SInt = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        SInt( (multOutputBitWidth + log2Up(config.multPerPe * (outerIndex.c * config.localVectorPeCol + config.localVectorPeCol))) bits)
      case Dataflow.ReuseB =>
        SInt( (multOutputBitWidth + log2Up(config.multPerPe * (outerIndex.r * config.localVectorPeRow + config.localVectorPeRow))) bits)
      case Dataflow.ReuseC =>
        SInt( systolicOutputBitwidth bits )
    }
  }

  // The array's C port: one whole fabric pass, so position-independent by construction.
  override def createSystolicOutputTypeC: SInt = {
    SInt( systolicOutputBitwidth bits)
  }

  // Only reachable for a ReuseA/ReuseB accumulator config — `accumulatorOutputBitWidth`
  // throws for ReuseC, which has no external accumulator to widen for.
  override def createAccumulatorOutputTypeC: SInt = {
    SInt( accumulatorOutputBitWidth bits )
  }

}

/**
 * [[PortTypeProvider]] for a `NumericType.Unsigned` config.
 *
 * Every width formula is character-for-character the Signed one — see
 * [[SignedPortTypeProvider]] for what they mean. Only the constructed type differs, so
 * a change to any width must be made in BOTH classes. (They are duplicated rather than
 * shared because `SInt` and `UInt` have no common supertype that `AccType` could be
 * bound to while still admitting `S(0, w bits)` / `U(0, w bits)`.)
 */
class UnsignedPortTypeProvider(val config: SystolicArrayConfig) extends PortTypeProvider[UInt, UInt]{

  override def createInputTypeA: UInt = UInt(config.bitWidthA bits)
  override def createInputTypeB: UInt = UInt(config.bitWidthB bits)
  override def createMultOutputType: UInt = UInt(multOutputBitWidth bits)
  override protected def createAccumulationType(width: Int): UInt = UInt(width bits)

  override def createVectorPeInputTypeC(outerIndex: OuterPeIndex, innerIndex: InnerIndex): UInt = {

    // NOTE: asymmetric with SignedPortTypeProvider, which has no such guard here — and
    // with this class's own three other C-port methods, none of which restrict the
    // config. As written it rejects every Unsigned GroupPE / ClusterPE config (any
    // localVectorPe* > 1) at elaboration, even though the formula below handles the
    // non-standalone indices perfectly well. Looks like a leftover rather than a real
    // constraint; kept as-is because removing it is a behaviour change, not a comment.
    config.requireStandAlone()

    config.dataflow match {
      case Dataflow.ReuseA =>
        UInt((multOutputBitWidth + log2Up((config.multPerPe * (outerIndex.c * config.localVectorPeCol + innerIndex.b)).max(1))) bits)
      case Dataflow.ReuseB =>
        UInt((multOutputBitWidth + log2Up((config.multPerPe * (outerIndex.r * config.localVectorPeRow + innerIndex.a)).max(1))) bits)
      case Dataflow.ReuseC =>
        UInt(systolicOutputBitwidth bits)

    }

  }


  override def createVectorPeOutputTypeC(outerIndex: OuterPeIndex, innerIndex: InnerIndex): UInt = {

    config.dataflow match {
      case Dataflow.ReuseA =>
        UInt( (multOutputBitWidth + log2Up(config.multPerPe * (outerIndex.c * config.localVectorPeCol + innerIndex.b + 1))) bits)

      case Dataflow.ReuseB =>
        UInt( (multOutputBitWidth + log2Up(config.multPerPe * (outerIndex.r * config.localVectorPeRow + innerIndex.a + 1))) bits)

      case Dataflow.ReuseC =>
        UInt( systolicOutputBitwidth bits )

    }
  }


  override def createOuterPeInputTypeC(outerIndex: OuterPeIndex): UInt = {

    config.dataflow match {
      case Dataflow.ReuseA =>
        UInt( (multOutputBitWidth + log2Up((config.multPerPe * outerIndex.c * config.localVectorPeCol).max(1))) bits)

      case Dataflow.ReuseB =>
        UInt( (multOutputBitWidth + log2Up((config.multPerPe * outerIndex.r * config.localVectorPeRow).max(1))) bits)

      case Dataflow.ReuseC =>
        UInt( systolicOutputBitwidth bits )

    }
  }

  override def createOuterPeOutputTypeC(outerIndex: OuterPeIndex): UInt = {

    config.dataflow match {
      case Dataflow.ReuseA =>
        UInt( (multOutputBitWidth + log2Up(config.multPerPe * (outerIndex.c * config.localVectorPeCol + config.localVectorPeCol))) bits)
      case Dataflow.ReuseB =>
        UInt( (multOutputBitWidth + log2Up(config.multPerPe * (outerIndex.r * config.localVectorPeRow + config.localVectorPeRow))) bits)
      case Dataflow.ReuseC =>
        UInt( systolicOutputBitwidth bits )
    }
  }

  override def createSystolicOutputTypeC: UInt = {
    UInt( systolicOutputBitwidth bits)
  }

  override def createAccumulatorOutputTypeC: UInt = {
    UInt( accumulatorOutputBitWidth bits )
  }

}