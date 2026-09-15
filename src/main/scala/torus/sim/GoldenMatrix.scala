package torus.sim

import torus.config._
import torus.core._

/**
 * The software reference model: it invents a GEMM, says what the answer is, and hands the
 * driver the operands ALREADY IN THE ORDER the hardware wants them.
 *
 * Every simulation asks it three things — `inputA` / `inputB` (the stimulus), `referenceC`
 * (what the whole GEMM should produce), and [[shapedTiles]] (the per-tile work list).
 *
 * The load-bearing property is in `shapedTiles`: each tile's `partialC` is computed from
 * the NATURAL tile, before any shaping, while its `a` and `b` are the shaped operands. So
 * the expectation is derived independently of the transform being tested. Shaping is the
 * part most likely to be wrong, and computing the golden from the shaped tile instead
 * would bake any error straight into the expectation and make the test pass on broken
 * shaping. Keep that split if this class is ever refactored.
 *
 * A tile goes through three stages, in this order, and they answer different questions:
 *
 *  1. `prePadForReuseC` — grow the tile so the HARDWARE PROTOCOL is satisfied at all.
 *     Grouped ReuseC pads K because its accEnableC gate is ANDed with its own previous
 *     cycle and so never fires on a 1-cycle pulse; Toroidal ReuseC pads to a square
 *     because the wrap rotates operands through a symmetric loop.
 *  2. `shapeTileForTest` — the reordering proper: diagonal skew, block transpose and
 *     flips, composed from [[Matrix]] one line per (fabric, dataflow).
 *  3. `padForReuse` — top up the STREAMED tile along the contraction axis by COPYING
 *     (not zeroing) so the array keeps seeing valid operands while the stationary one is
 *     still in use.
 *
 * [[tileShape]] is where the tiles get their size, and it reads `contractionPassDepth`
 * rather than any array dimension of its own — that is the single source shared with the
 * accumulator width, so a tile can never out-run the hardware provisioned to sum it.
 *
 * DUAL MODE is deliberately only half-handled here: `shapeTileForTest` passes a tile
 * through untouched and [[DualModeDriver]] owns its own golden, because a broadcast
 * stationary operand plus per-PE streamed lanes has no skew to apply. The rest of the
 * class is normal-GEMM only, which is why `tileShapeFor`, `prePadForReuseC` and
 * `dataflowOf` do not match `ToroidalDualModeConfig` — the three "match may not be
 * exhaustive" warnings this file compiles with are that decision, not an oversight.
 */
object GoldenMatrix {
  type MatrixPair = (Matrix, Matrix)

  final case class TileIndex(m: Int, n: Int, k: Int)

  final case class TileShape(tileM: Int, tileN: Int, tileK: Int)

  final case class ShapedTile(
    index:    TileIndex,
    a:        Matrix,
    b:        Matrix,
    partialC: Matrix,
  )

}

class GoldenMatrix(gemmDim: GemmDim, config: SystolicArrayConfig) {

  import GoldenMatrix._

  private case class InputGemmMatrices(a: Matrix, b: Matrix)

  /** A natural-order tile with its grid coordinate (pre-shaping). */
  private case class TileEntry(index: TileIndex, mats: InputGemmMatrices)

  private val gemmMatrices: InputGemmMatrices = generateMatrix(gemmDim)

  def inputA: Matrix = gemmMatrices.a
  def inputB: Matrix = gemmMatrices.b

  lazy val referenceC: Matrix =
    matMul(gemmMatrices.a, gemmMatrices.b)

  /** The tile geometry every tile in [[shapedTiles]] was cut with. */
  lazy val tileShape: TileShape = tileShapeFor

  private lazy val naturalTiles: Seq[TileEntry] =
    tileMatrices(gemmMatrices, tileShape)

  def referenceCBlock(m: Int, n: Int): Matrix = {
    val parts = naturalTiles
      .filter(e => e.index.m == m && e.index.n == n)
      .sortBy(_.index.k)
      .map(e => matMul(e.mats.a, e.mats.b))

    if (parts.isEmpty) Matrix.empty
    else parts.tail.foldLeft(parts.head)(addMatrix)
  }

  // `partial` is computed from the NATURAL tile, before shaping — see the class header.
  // Deriving it from `a` and `b` below would make a shaping bug invisible.
  lazy val shapedTiles: Seq[ShapedTile] =
    naturalTiles.map { e =>
      val partial  = matMul(e.mats.a, e.mats.b)
      val prepped  = prePadForReuseC(e.mats)
      val s        = shapeTileForTest(prepped)
      val (a, b)   = padForReuse(s.a, s.b)
      ShapedTile(e.index, a, b, partial)
    }

  private def prePadForReuseC(t: InputGemmMatrices): InputGemmMatrices =
    dataflowOf match {
      case Dataflow.ReuseC =>
        config match {
          case _: GroupedConfig =>
            // Grouped ReuseC: zero-pad K only (a padCols, b padRows) so streamCycles >= 2.
            // HW gates accEnableC with its previous-cycle value, so a 1-cycle pulse never fires.
            // M/N are already <= totalVectorPe* and need no padding.
            val kPad = config.systolicArrayColDimension
            InputGemmMatrices(
              a = padCols(t.a, kPad),
              b = padRows(t.b, kPad),
            )
          case _: ToroidalConfig =>
            // Toroidal ReuseC: zero-pad to a square n×n tile (rowDim == colDim here),
            // padding all of M, K, N. a(M×K): padRows=M, padCols=K. b(K×N): padRows=K, padCols=N.
            // Pad K to the wrap-rotation capacity (cluster-loop length × mult), the same
            // bound tileShapeFor uses for tileK; broadcast M/N grow via totalVectorPe*.
            val kPad = config.outerPeRow * config.multPerPe
            InputGemmMatrices(
              a = padCols(padRows(t.a, config.totalVectorPeRow), kPad),
              b = padCols(padRows(t.b, kPad), config.totalVectorPeCol),
            )
        }
      case _ => t
    }

  // ─────────────────────────────────────────────────────────────
  // Reuse padding:
  //   ReuseA: if completed tileB.rows < systolicArrayRowDimension,
  //           copy the top `multPerPe` rows and concat at the bottom.
  //   ReuseB: if completed tileA.cols < systolicArrayColDimension,
  //           copy the left `multPerPe` cols and concat at the right.
  // ─────────────────────────────────────────────────────────────
  private def padForReuse(a: Matrix, b: Matrix): MatrixPair = dataflowOf match {
    case Dataflow.ReuseA =>
      config match {
        case _: ToroidalConfig =>
          // The streamed tile B is padded along its contraction (K) dimension,
          // which physically maps to the array COLUMNS for ReuseA, so the target
          // is the column (contraction) capacity — NOT the row (output M) capacity.
          // For localVectorPeRow==1 the constraint outerPeCol*lvC==outerPeRow makes
          // the two equal; for lvR>1 (broadcast) rowDim = colDim*lvR, so only the
          // column capacity is correct.
          val target = config.systolicArrayColDimension
//          (a, padRowsByCopyTop(b, target))
          val copyDepth = math.min(target, b.rows + a.rows - config.multPerPe)
          (a, padRowsByCopyTop(b, copyDepth))
        case _ =>
          (a, b)
      }
    case Dataflow.ReuseB =>
      config match {
        case _: ToroidalConfig =>
//          require(
//            b.cols <= config.systolicArrayColDimension,
//            s"[error] completed tileB.cols (${b.cols}) must be <= systolicArrayColDimension (${config.systolicArrayColDimension})"
//          )
          // The streamed tile A is padded along its contraction (K) dimension,
          // which physically maps to the array ROWS for ReuseB, so the target is
          // the row (contraction) capacity — NOT the column (output N) capacity.
          // For localVectorPeCol==1 the constraint outerPeRow*lvR==outerPeCol makes
          // the two equal; for lvC>1 (broadcast) colDim = rowDim*lvC, so only the
          // row capacity is correct.
          val target = config.systolicArrayRowDimension
          val copyDepth = math.min(target, a.cols + b.cols - config.multPerPe)

          (padColsByCopyLeft(a, copyDepth), b)
        case _ =>
          (a, b)
      }
    case Dataflow.ReuseC =>
      (a,b)
  }

  private def padRows(m: Matrix, target: Int): Matrix = {
    if (m.rows >= target || m.isEmpty) m
    else {
      val width = m.cols
      val extra = Array.fill(target - m.rows)(Array.fill(width)(0))
      Matrix(m.data ++ extra)
    }
  }

  private def padCols(m: Matrix, target: Int): Matrix = {
    if (m.cols >= target || m.isEmpty) m
    else {
      val newData = m.data.map { rowArr =>
        rowArr ++ Array.fill(target - rowArr.length)(0)
      }
      Matrix(newData)
    }
  }

  // COPY-pad, not zero-pad: reuse padding tops a tile up by repeating its own leading
  // rows/cols cyclically. Zeros would be valid data the array multiplies and accumulates;
  // repeated real values land in slots whose results are discarded, so nothing the test
  // reads is affected either way — but the operand stream stays continuous.
  private def padRowsByCopyTop(m: Matrix, target: Int): Matrix = {
    if (m.rows >= target || m.isEmpty) m
    else {
      val need  = target - m.rows
      val extra = Array.tabulate(need) { i => m.row(i % m.rows).clone() }
      Matrix(m.data ++ extra)
    }
  }

  private def padColsByCopyLeft(m: Matrix, target: Int): Matrix = {
    if (m.cols >= target || m.isEmpty) m
    else {
      val newData = m.data.map { rowArr =>
        val need  = target - rowArr.length
        val extra = Array.tabulate(need) { j => rowArr(j % rowArr.length) }
        rowArr ++ extra
      }
      Matrix(newData)
    }
  }

  private def generateMatrix(gemmDim: GemmDim): InputGemmMatrices =
    InputGemmMatrices(
      a = Matrix.random(gemmDim.M, gemmDim.K),
      b = Matrix.random(gemmDim.K, gemmDim.N)
    )

  private def ceilDiv(x: Int, y: Int): Int = (x + y - 1) / y

  private def matMul(a: Matrix, b: Matrix): Matrix = {
    if (a.isEmpty || b.isEmpty) Matrix.empty
    else Matrix.tabulate(a.rows, b.cols) { (i, j) =>
      var acc = 0
      var k = 0
      while (k < a.cols) { acc += a(i, k) * b(k, j); k += 1 }
      acc
    }
  }

  private def addMatrix(x: Matrix, y: Matrix): Matrix = {
    require(
      x.rows == y.rows && x.cols == y.cols,
      s"[error] addMatrix shape mismatch: ${x.shapeStr} vs ${y.shapeStr}"
    )
    Matrix.tabulate(x.rows, x.cols)((i, j) => x(i, j) + y(i, j))
  }

  /**
   * How much of the FREE (non-contracted) output dimension one ReuseA/ReuseB tile
   * may span — N for ReuseA, M for ReuseB.
   *
   * The only thing that bounds it is the accumulator: every slot of a K-pass must
   * fit in the AccSram address space, whose depth is `freeDimensionSize`. Without
   * an accumulator (`needAccumulator == false`) nothing bounds it, so a tile takes
   * the whole GEMM extent.
   *
   * Grouped reserves the LAST slot of every panel as a TypeC de-skew zero-flush
   * (`tilesPerPanel = numSlots - 1` in [[DataflowDriver]]): its final real rows are
   * still walking the de-skew chain when the burst ends, and only clock into the
   * accumulator while that trailing slot streams zeros. Toroidal's PE outputs are
   * immediate and reserve nothing.
   *
   * The reserve costs one whole SLOT, so the cheapest schedule is the one with the
   * SMALLEST slot that still covers the de-skew — not the largest tile that leaves a
   * slot free. Picking `depth / 2` (one slot of data, one of flush) spends half the
   * accumulator on the flush; picking the smallest divisor >= [[accDeskewSpan]]
   * spends only that. Both satisfy the driver's `depth % slotStride == 0`.
   */
  private def freeDimensionCap: Int =
    if (!config.needAccumulator) Int.MaxValue
    else if (config.isGrouped) {
      val depth = config.freeDimensionSize
      (1 to depth)
        .find(slot => depth % slot == 0 && slot >= accDeskewSpan && depth / slot >= 2)
        .getOrElse(depth / 2) // no divisor covers the span: fall back to the old halving
    }
    else config.freeDimensionSize

  /**
   * Cycles the TypeC de-skew chain spreads the output ports over, which is what a
   * zero-flush slot has to cover. [[AccTapCell]] delays cell `i`'s control by
   * `accTapCellDepth + (numCells - i)`, so the spread across cells is `numCells`
   * (= one AccTap cell per group of `numAccTapCellPort` output ports). The common
   * `accTapCellDepth` / prelude latency shifts every port equally and does not widen
   * the spread. Grouped ReuseA/ReuseB only — the sole path that reaches it.
   */
  private def accDeskewSpan: Int =
    config.topology.systolicOutputDimC / config.topology.numAccTapCellPort

  private def tileShapeFor: TileShape = config match {
    case grouped: GroupedConfig =>
      grouped.dataflow match {
        // tileK is `contractionPassDepth` for every ReuseA/ReuseB tile: the contraction
        // is SPATIAL there, summed down the array's column (ReuseA) or row (ReuseB)
        // adder chain, so one pass through the fabric consumes exactly that many K. It
        // is the same figure the accumulator data width is sized from
        // (`contractionPasses`), so reading it here keeps the tile and the width from
        // drifting apart.
        case Dataflow.ReuseA => TileShape(
          tileM = math.min(config.totalVectorPeRow, gemmDim.M),
          tileN = math.min(freeDimensionCap, gemmDim.N),
          tileK = math.min(config.contractionPassDepth, gemmDim.K),
        )
        case Dataflow.ReuseB => TileShape(
          tileM = math.min(freeDimensionCap, gemmDim.M),
          tileN = math.min(config.totalVectorPeCol,gemmDim.N),
          tileK = math.min(config.contractionPassDepth, gemmDim.K)
        )
        // Output-stationary on Grouped: M and N are SPATIAL (they are input ports), but
        // K is TEMPORAL — it is the number of cycles `accEnableC` is held high while the
        // operands stream (see runGroupedReuseC). Nothing in the fabric bounds a cycle
        // count, so the bound is the PE accumulator width, i.e. exactly the K the config
        // declares. (Toroidal ReuseC is different: the wrap recirculates, so one burst
        // really is capped at the cluster-loop length — see below.)
        case Dataflow.ReuseC => TileShape(
          tileM = math.min(config.totalVectorPeRow, gemmDim.M),
          tileN = math.min(config.totalVectorPeCol, gemmDim.N),
          tileK = math.min(config.maxContractionSize, gemmDim.K),
        )
      }

    case toroidal: ToroidalConfig =>
      toroidal.dataflow match {
        case Dataflow.ReuseA =>
          // tileM is the M output width = totalVectorPeRow (number of output rows),
          // which must NOT include multPerPe (that factor is contraction, folded
          // inside each PE). systolicArrayRowDimension = totalVectorPeRow*mult would
          // over-size the output tile for mult>1. Equal when mult==1.
          val tileM = math.min(gemmDim.M, config.totalVectorPeRow)
          val tileN = math.min(gemmDim.N, freeDimensionCap)
          val tileK = math.min(gemmDim.K, config.contractionPassDepth)
          TileShape(
            tileM = tileM,
            tileN = tileN,
            tileK = tileK,
          )

        case Dataflow.ReuseB =>
          val tileM = math.min(gemmDim.M, freeDimensionCap)
          // tileN is the N output width = totalVectorPeCol (number of output cols),
          // which must NOT include multPerPe. systolicArrayColDimension =
          // totalVectorPeCol*mult would over-size the output tile for mult>1.
          // Equal when mult==1.
          val tileN = math.min(gemmDim.N, config.totalVectorPeCol)
          val tileK = math.min(gemmDim.K, config.contractionPassDepth)
          TileShape(
            tileM = tileM,
            tileN = tileN,
            tileK = tileK,
          )

        case Dataflow.ReuseC =>
          // Per-tile contraction depth is set by how many K values the wrap rotation
          // can recirculate, which is the CLUSTER-loop length (outerPeCol hops for A,
          // outerPeRow for B), each carrying multPerPe lanes — NOT the physical row
          // count. Broadcast (lvR/lvC > 1) adds parallel output rows/cols, not K depth,
          // so lvR/lvC must not inflate tileK. Clusters are square (outerPeRow ==
          // outerPeCol), and for lvR == lvC == 1 this equals the old systolic bound.
          TileShape(
            tileM = math.min(config.totalVectorPeRow, gemmDim.M),
            tileN = math.min(config.totalVectorPeCol, gemmDim.N),
            // Single source: `contractionPassDepth` is the same cluster-loop length the
            // PE output width is sized from, so the tile can never out-run the width.
            tileK = math.min(config.contractionPassDepth, gemmDim.K)
          )
      }
  }

  private def dataflowOf: Dataflow.Value = config match {
    case grouped: GroupedConfig => grouped.dataflow
    case toroidal: ToroidalConfig => toroidal.dataflow
  }

  // The EMISSION ORDER differs per dataflow, and it is not cosmetic: it is the order the
  // driver streams tiles in, so it decides which index varies fastest and therefore what
  // stays resident between tiles. ReuseA walks (m, k, n) so the stationary A tile is held
  // across the n sweep, ReuseB walks (k, n, m) to hold B, and ReuseC walks (m, n, k) so
  // the whole k run accumulates into one output tile before moving on.
  //TODO is this right?
  private def tileMatrices(
    gemmMatrices: InputGemmMatrices,
    shape: TileShape
  ): Seq[TileEntry] = {
    val numM = ceilDiv(gemmMatrices.a.rows, shape.tileM)
    val numK = ceilDiv(gemmMatrices.a.cols, shape.tileK)
    val numN = ceilDiv(gemmMatrices.b.cols, shape.tileN)

    val tiledA = gemmMatrices.a.split(shape.tileM, shape.tileK)
    val tiledB = gemmMatrices.b.split(shape.tileK, shape.tileN)

    def pick(m: Int, n: Int, k: Int): TileEntry = TileEntry(
      index = TileIndex(m, n, k),
      mats  = InputGemmMatrices(
        a = tiledA(m * numK + k),
        b = tiledB(k * numN + n)
      )
    )

    dataflowOf match {
      case Dataflow.ReuseA =>
        for { m <- 0 until numM; k <- 0 until numK; n <- 0 until numN } yield pick(m, n, k)
      case Dataflow.ReuseB =>
        for { k <- 0 until numK; n <- 0 until numN; m <- 0 until numM } yield pick(m, n, k)
      case Dataflow.ReuseC =>
        for { m <- 0 until numM; n <- 0 until numN; k <- 0 until numK } yield pick(m, n, k)
    }
  }

  private def shapeTileForTest(tile: InputGemmMatrices): InputGemmMatrices = config match {
    case grouped: GroupedConfig =>
      grouped.dataflow match {
        case Dataflow.ReuseA =>
          InputGemmMatrices(
            a = tile.a.blockTransposeRow(grouped.multPerPe).flipCol,
            b = tile.b.transpose
          )

        case Dataflow.ReuseB =>
          InputGemmMatrices(
            a = tile.a.transpose,
            b = tile.b.blockTransposeCol(grouped.multPerPe).flipRow
          )

        case Dataflow.ReuseC =>
          InputGemmMatrices(
            a = tile.a.blockTransposeRow(grouped.multPerPe),
            b = tile.b.blockTransposeCol(grouped.multPerPe)
          )
      }
    case toroidal: ToroidalConfig =>
      toroidal.dataflow match {
        case Dataflow.ReuseA =>
          // Broadcast ReuseA: the lvR output M-rows in one cluster-row share the
          // streamed-B timing, so the stationary-A diagonal skew is grouped by
          // localVectorPeRow (== plain shiftLeft when lvR == 1).
          InputGemmMatrices(
            a = tile.a.shiftLeftGrouped(toroidal.multPerPe, toroidal.localVectorPeRow).blockTransposeRow(toroidal.multPerPe).flipCol,
            b = tile.b
          )
        case Dataflow.ReuseB =>
          // Broadcast ReuseB: the lvC output N-columns in one cluster-column share
          // the broadcast activation timing, so the stationary-B diagonal skew is
          // grouped by localVectorPeCol (== plain shiftUp when lvC == 1).
          InputGemmMatrices(
            a = tile.a,
            b = tile.b.shiftUpGrouped(toroidal.multPerPe, toroidal.localVectorPeCol).blockTransposeCol(toroidal.multPerPe).flipRow
          )
        case Dataflow.ReuseC =>
          // Doubly-systolic OS: A carries a per-M-row diagonal skew, B a per-N-column
          // one. Under broadcast the lvR M-rows in a cluster-row share B's timing and
          // the lvC N-columns in a cluster-column share A's timing, so both skews are
          // grouped (== plain shiftLeft/shiftUp when lvR == lvC == 1).
          InputGemmMatrices(
            a = tile.a.shiftLeftGrouped(toroidal.multPerPe, toroidal.localVectorPeRow).blockTransposeRow(toroidal.multPerPe).flipCol,
            b = tile.b.shiftUpGrouped(toroidal.multPerPe, toroidal.localVectorPeCol).blockTransposeCol(toroidal.multPerPe).flipRow
          )
      }

    // Toroidal DualMode is driven by [[DualModeDriver]], which owns its own
    // low-M GEMV golden: the stationary operand is BROADCAST across the whole
    // stationary axis (so no diagonal skew is possible or needed) and the streamed one
    // rides dedicated per-VectorPE lanes (so no systolic skew either). Both operands
    // are therefore passed through unshaped. The rest of this class (tileShapeFor,
    // prePadForReuseC, dataflowOf) is still normal-GEMM only and will not match a
    // DualMode config.
    case _: ToroidalDualModeConfig => tile
  }

}
