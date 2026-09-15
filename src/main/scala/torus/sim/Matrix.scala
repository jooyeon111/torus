package torus.sim

import scala.util.Random

/**
 * A plain integer matrix for the SIMULATION side — the software half of every test.
 *
 * Nothing here is hardware. It is the value type [[GoldenMatrix]] computes references
 * with and, more importantly, a vocabulary of RESHAPING operations: `transpose`,
 * `flipRow` / `flipCol`, `blockTranspose*`, `shift*` and `split`. A systolic array does
 * not want a tile in natural row-major order — it wants it skewed, interleaved and
 * reversed so that each element arrives at the right PE on the right cycle — and
 * `GoldenMatrix.shapeTileForTest` composes these methods into exactly that order, one
 * pipeline per (fabric, dataflow). Keeping the transforms here and the choice of which to
 * apply there is what makes those pipelines readable as one line each.
 *
 * Two things that are easy to trip over:
 *
 *  - `equals` / `hashCode` are overridden BECAUSE this is a case class over `Array`.
 *    Scala's generated versions would compare arrays by reference, so two structurally
 *    identical matrices would differ and every golden comparison would fail. The
 *    element-wise versions below are what make `==` mean what the tests assume.
 *  - `random` draws from 1..5 — small, and never ZERO. Small keeps products from
 *    swamping a narrow accumulator and turning a real mismatch into an overflow; non-zero
 *    matters because zero is the padding value everywhere in this package, so a stray
 *    zero from stimulus would be indistinguishable from a pad and could hide a
 *    misalignment that shifts real data into a pad slot.
 */
object Matrix {
  def empty: Matrix = Matrix(Array.empty[Array[Int]])

  def random(rows: Int, cols: Int): Matrix =
    Matrix(Array.fill(rows, cols)(Random.nextInt(5) + 1))

  def tabulate(rows: Int, cols: Int)(f: (Int, Int) => Int): Matrix =
    Matrix(Array.tabulate(rows, cols)(f))
}

case class Matrix(data: Array[Array[Int]]) {

  def rows: Int = data.length
  def cols: Int = if (data.nonEmpty) data(0).length else 0
  def isEmpty: Boolean = data.isEmpty || (data.nonEmpty && data(0).isEmpty)
  def nonEmpty: Boolean = !isEmpty
  def apply(r: Int, c: Int): Int = data(r)(c)

  def row(r: Int): Array[Int] = data(r)

  def nonZeroValues: Iterator[Int] = data.iterator.flatMap(_.iterator).filter(_ != 0)

  def shapeStr: String = s"${rows}x$cols"

  override def equals(obj: Any): Boolean = obj match {
    case that: Matrix =>
      if (this.rows != that.rows || this.cols != that.cols) false
      else (0 until rows).forall { r =>
        java.util.Arrays.equals(this.data(r), that.data(r))
      }
    case _ => false
  }
  override def hashCode(): Int =
    data.foldLeft(1)((acc, row) => 31 * acc + java.util.Arrays.hashCode(row))
  override def toString: String = s"Matrix(${rows}x$cols)"

  def print(): Unit = {
    if (isEmpty) { println("  (empty)"); return }
    val maxWidth = data.flatten.map(_.toString.length).max
    data.foreach { row =>
      println("  " + row.map(v => s"%${maxWidth}d".format(v)).mkString(", "))
    }
  }

  def print(label: String): Unit = {
    println(s"   $label ($shapeStr):")
    if (isEmpty) println("     (empty)")
    else data.foreach { row => println("     " + row.mkString(", ")) }
  }

  def transpose: Matrix = {
    if (isEmpty) Matrix.empty
    else Matrix(Array.tabulate(cols, rows) { (c, r) => data(r)(c) })
  }

  def flipRow: Matrix = Matrix(data.reverse)
  def flipCol: Matrix = Matrix(data.map(_.reverse))

  // Cut into tiles in ROW-MAJOR tile order, zero-padding the last row and column of tiles
  // out to a full tile. Callers index the result as `tr * numTileCols + tc`; GoldenMatrix
  // relies on that flat order to pair an A tile with its B tile.
  def split(tileRows: Int, tileCols: Int): Seq[Matrix] = {
    val origRows = rows
    val origCols = cols

    val paddedRows = ((origRows + tileRows - 1) / tileRows) * tileRows
    val paddedCols = ((origCols + tileCols - 1) / tileCols) * tileCols

    val padded = Array.tabulate(paddedRows, paddedCols) { (r, c) =>
      if (r < origRows && c < origCols) data(r)(c) else 0
    }

    val numTileRows = paddedRows / tileRows
    val numTileCols = paddedCols / tileCols

    for {
      tr <- 0 until numTileRows
      tc <- 0 until numTileCols
    } yield Matrix(Array.tabulate(tileRows, tileCols) { (r, c) =>
      padded(tr * tileRows + r)(tc * tileCols + c)
    })
  }

  /**
   * For each row, group elements into chunks of size `p` and emit p new rows
   * containing the i-th element of each chunk. Zero-pads columns to a
   * multiple of `p`.
   */
  def blockTransposeRow(p: Int): Matrix = {
    if (isEmpty) return Matrix.empty
    val c = cols
    val paddedCols = if (c % p == 0) c else ((c / p) + 1) * p
    val padded = data.map { row =>
      row ++ Array.fill(paddedCols - row.length)(0)
    }
    val result = padded.flatMap { row =>
      val groups = row.grouped(p).toArray
      (0 until p).map { i => groups.map(_(i)) }
    }
    Matrix(result)
  }

  /**
   * Group rows into blocks of `p` rows; for each block, emit `cols` new rows
   * where each new row contains the p values down one column of the block.
   * Zero-pads rows to a multiple of `p`.
   */
  def blockTransposeCol(p: Int): Matrix = {
    if (isEmpty) return Matrix.empty
    val r = rows
    val c = cols
    val paddedRows = if (r % p == 0) r else ((r / p) + 1) * p
    val padded = data ++ Array.fill(paddedRows - r)(Array.fill(c)(0))
    val numBlocks = paddedRows / p
    val result = (0 until numBlocks).map { b =>
      val blockRows = padded.slice(b * p, (b + 1) * p)
      (0 until c).flatMap { col => blockRows.map(_(col)) }.toArray
    }.toArray
    Matrix(result)
  }

  /**
   * Rotate each column UP by `(colIdx * bp) mod paddedRows`, after padding the rows out
   * to a multiple of `bp`.
   *
   * This is the diagonal skew a systolic array needs: column `c` enters `bp` cycles later
   * than column `c-1`, so staggering the data by the same amount makes every element meet
   * its partner on the right cycle. The rotation is MODULAR, not a shift-with-fill — what
   * falls off the top comes back at the bottom, which is what the wrap recirculation
   * expects.
   */
  def shiftUp(bp: Int): Matrix = {
    if (isEmpty) return this
    val r = rows
    val c = cols
    val paddedRows = if (r % bp == 0) r else r + (bp - r % bp)
    val padded = Array.tabulate(paddedRows, c) { (rr, cc) =>
      if (rr < r) data(rr)(cc) else 0
    }
    val result = Array.ofDim[Int](paddedRows, c)
    for (cc <- 0 until c) {
      val shift = (cc * bp) % paddedRows
      for (rr <- 0 until paddedRows) {
        result(rr)(cc) = padded((rr + shift) % paddedRows)(cc)
      }
    }
    Matrix(result)
  }

  /**
   * Like [[shiftUp]], but columns are grouped in blocks of `group` consecutive
   * columns that all share the SAME shift `((colIdx / group) * bp) mod paddedRows`.
   *
   * Toroidal broadcast ReuseB: the `localVectorPeCol` output N-columns that live in one
   * physical cluster-column receive the same activation timing (broadcast), so
   * their stationary weights must carry the same diagonal skew (the cluster
   * column's skew), not a per-N-column skew. With `group == 1` this is identical
   * to [[shiftUp]].
   */
  def shiftUpGrouped(bp: Int, group: Int): Matrix = {
    if (isEmpty) return this
    val r = rows
    val c = cols
    val paddedRows = if (r % bp == 0) r else r + (bp - r % bp)
    val padded = Array.tabulate(paddedRows, c) { (rr, cc) =>
      if (rr < r) data(rr)(cc) else 0
    }
    val result = Array.ofDim[Int](paddedRows, c)
    for (cc <- 0 until c) {
      val shift = ((cc / group) * bp) % paddedRows
      for (rr <- 0 until paddedRows) {
        result(rr)(cc) = padded((rr + shift) % paddedRows)(cc)
      }
    }
    Matrix(result)
  }

  /** The transpose of [[shiftUp]]: rotate each row LEFT by `(rowIdx * ap) mod paddedCols`,
   * after padding the columns out to a multiple of `ap`. Same modular rotation, applied
   * on the other axis.
   */
  def shiftLeft(ap: Int): Matrix = {
    if (isEmpty) return this
    val r = rows
    val c = cols
    val paddedCols = if (c % ap == 0) c else c + (ap - c % ap)
    val padded = Array.tabulate(r, paddedCols) { (rr, cc) =>
      if (cc < c) data(rr)(cc) else 0
    }
    val result = Array.ofDim[Int](r, paddedCols)
    for (rr <- 0 until r) {
      val shift = (rr * ap) % paddedCols
      for (cc <- 0 until paddedCols) {
        result(rr)(cc) = padded(rr)((cc + shift) % paddedCols)
      }
    }
    Matrix(result)
  }

  /**
   * Like [[shiftLeft]], but rows are grouped in blocks of `group` consecutive
   * rows that all share the SAME shift `((rowIdx / group) * ap) mod paddedCols`.
   *
   * Toroidal broadcast ReuseA: the `localVectorPeRow` output M-rows that live in one
   * physical cluster-row share the same streamed-B timing (broadcast), so their
   * stationary weights must carry the same diagonal skew (the cluster row's
   * skew). With `group == 1` this is identical to [[shiftLeft]].
   */
  def shiftLeftGrouped(ap: Int, group: Int): Matrix = {
    if (isEmpty) return this
    val r = rows
    val c = cols
    val paddedCols = if (c % ap == 0) c else c + (ap - c % ap)
    val padded = Array.tabulate(r, paddedCols) { (rr, cc) =>
      if (cc < c) data(rr)(cc) else 0
    }
    val result = Array.ofDim[Int](r, paddedCols)
    for (rr <- 0 until r) {
      val shift = ((rr / group) * ap) % paddedCols
      for (cc <- 0 until paddedCols) {
        result(rr)(cc) = padded(rr)((cc + shift) % paddedCols)
      }
    }
    Matrix(result)
  }
}


