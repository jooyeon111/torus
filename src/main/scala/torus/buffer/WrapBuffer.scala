package torus.buffer

import spinal.core._

import torus.arithmetic._
import torus.config._
import torus.core._

/**
 * The operand-entry buffer of a Toroidal ReuseC array: one [[WrapBufferCell]] per
 * PHYSICAL row (TypeA) or column (TypeB), each closing that line of the fabric into a
 * ring.
 *
 * It sits in the same [[SkewBufferIO]] slot the other two buffers use — the array holds
 * it as `preprocessorA` / `preprocessorB` and `wireA` / `wireB` never branch on which
 * buffer it is — but it carries two ports they do not:
 * {{{
 *   input         fresh operands from the array port
 *   rotatedInput  the far edge's outputs, wired straight back here by the array
 *   selInput      per line: stream fresh (True) or recirculate (False)
 *   output        what the fabric's near edge actually reads
 * }}}
 * The array closes the loop, not this module: `ToroidalSystolicArray.wireA` drives
 * `rotatedInput` from the rightmost cluster column's `outputA` (and `wireB` from the
 * bottom row's `outputB`), then reads `output` back into the fabric. So this is the hinge
 * of the torus — the place the two ends of each line meet — and the cells' registers are
 * what keep that loop sequential rather than combinational.
 *
 * Why a whole module of one-line cells: the recirculation gate is per LINE, so the array
 * needs `numCell` independent selects, and cutting the bus into cells is what gives each
 * line its own. It also keeps this level pure fan-out — flat index `i * cellPort + p`
 * into cell `i` lane `p` — so the array can address the bus as
 * `(physicalRow * multPerPe + lane)` with no translation.
 *
 * `numCell` counts PHYSICAL rows/columns (`totalVectorPeRow` / `totalVectorPeCol`), not
 * clusters: with broadcast (`localVectorPe* > 1`) one cluster spans several lines and
 * each still needs its own ring.
 */
class WrapBuffer[InputType <: Data, AccType <: Data](
  val tileType: TileType.Value,
  val config: SystolicArrayConfig,
)(
  implicit val portType: PortTypeProvider[InputType, AccType],
  implicit val arithmetic: Arithmetic[InputType, AccType],
) extends Component with SkewBufferIO[InputType] {

  // A ring only closes if the two axes have the same length — a non-square torus would
  // wrap row `r` onto a column that does not exist. `ToroidalConfig` enforces this for
  // ReuseC too; repeated here because this module is what would silently mis-wire.
  assert(config.outerPeRow == config.outerPeCol, "[error]")

  val toroidal = config.asToroidal
  toroidal.requireReuseC()

  private val bufferTypeName = "Wrap"
  setDefinitionName(s"${tileType}_${bufferTypeName}Buffer")

  private val topology   = config.topology
  private val cellPort   = topology.wrapBufferCellPort               // multPerPe lanes per line
  private val numCell    = topology.wrapBufferNumCell(tileType)      // one per physical row / column
  private def inputType  = portType.createInputTypeByTile(tileType)  // by-name: fresh type per port
  private val totalPorts = cellPort * numCell

  // Named bundle class rather than an anonymous `new Bundle`: `SkewBufferIO` fixes only
  // `input` / `output`, and the two extra ports have to be declared somewhere the
  // override can still satisfy that structural type.
  class WrapBufferBundle extends Bundle {
    val input        = in  Vec(HardType(inputType), totalPorts)
    val rotatedInput = in  Vec(HardType(inputType), totalPorts)
    val selInput     = in  Vec(Bool(), numCell)
    val output       = out Vec(HardType(inputType), totalPorts)
  }

  override val io = new WrapBufferBundle

  val cellVector = Vector.fill(numCell)(
    new WrapBufferCell[InputType, AccType](tileType, config)
  )

  // Pure fan-out: cell `i` owns the contiguous port window [i*cellPort, (i+1)*cellPort),
  // and both input buses are sliced with the SAME flat index so a lane's fresh value and
  // its recirculated value always meet in the same cell.
  for (i <- 0 until numCell) {
    for (p <- 0 until cellPort) {
      val flatIndex = i * cellPort + p
      cellVector(i).io.input(p) := io.input(flatIndex)
      cellVector(i).io.wrapedInput(p) := io.rotatedInput(flatIndex)
    }
    // One gate for the whole line — all its lanes stream or recirculate together.
    cellVector(i).io.selInput := io.selInput(i)

    for (p <- 0 until cellPort) {
      val flatIndex = i * cellPort + p
      io.output(flatIndex) := cellVector(i).io.output(p)
    }
  }
}
