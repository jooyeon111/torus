package torus.accumulator

import spinal.core._

import torus.arithmetic._
import torus.config._

/**
 * One BIG synchronous SRAM accumulator bank holding the partial sums of ALL
 * output ports at once (a "wide word" memory), as opposed to one small memory
 * per port.
 *
 *  - depth = `freeDimensionSize`
 *            (output-indexed: one entry per accumulator slot, spanning the free
 *            output dimension — N for ReuseA, M for ReuseB. The driver may split
 *            that address space into several stacked output tiles, so partial sums
 *            for several different output blocks can stay resident simultaneously,
 *            which is what lets ReuseB reuse a loaded B tile across every M-tile of
 *            an N-panel before draining)
 *  - word  = `numPorts` accumulator values concatenated
 *            (width = `numPorts * accumulatorOutputBitWidth`)
 *
 * The memory is a **1R1W synchronous** macro (one read port + one write port,
 * 1-cycle read latency), so the read-modify-write is pipelined over two cycles:
 * {{{
 *   cycle t   : issue read(addr)         ; latch addr / input / control
 *   cycle t+1 : rd = mem[addr] valid
 *               next = (sumEnable ? rd : 0) + input
 *               mem[addr] := next         (write port)
 * }}}
 * Because a slot is only revisited every `depth` cycles, the write of cycle t+1
 * is always committed long before that slot is read again (requires depth >= 2),
 * so no read-after-write bypass is needed. The simultaneous read (of the next
 * slot) and write (of the previous slot) target different addresses, which is
 * exactly what a 1R1W macro provides.
 *
 * Aligned-ports assumption: all `numPorts` share one address, so every port
 * must present the same accumulator slot on the same cycle (true for the Grouped
 * de-skewed ReuseA / ReuseB paths and the Toroidal ReuseB path this is wired into).
 *
 * Control is one [[AccControl]] plus an `outputValid` read strobe. The bundle arrives
 * BANK-LOCAL: the caller has already gated `psumValid` with this bank's role, so here
 * `psumValid` simply means "this bank steps a slot this cycle" and `firstLoop` means
 * "overwrite it rather than accumulate". Nothing here knows about panels or drains --
 * [[AccSramDoubleBuffer]] composes them, and a drain is just a psum pass with
 * `firstLoop` high and the input forced to zero, which overwrites every slot with 0.
 */
class AccSram[InputType <: Data, AccType <: Data](
  config: SystolicArrayConfig,
  numPorts: Int,
)(
  implicit val arithmetic: Arithmetic[InputType, AccType],
  implicit val portType: PortTypeProvider[InputType, AccType],
) extends Component {

  import arithmetic._

  val depth   = config.freeDimensionSize

  require(depth >= 2, s"[error] AccSram needs freeDimensionSize >= 2 for the pipelined sync RMW, got $depth")
  require(numPorts >= 1, s"[error] AccSram needs numPorts >= 1, got $numPorts")

  val io = new Bundle {
    val input       = in  Vec(portType.createSystolicOutputTypeC, numPorts)
    val accControl  = in  (AccControl())
    val outputValid = in  Bool()
    val output      = out Vec(portType.createAccumulatorOutputTypeC, numPorts)
  }

  // [[AccControl]] reduced to the two raw enables the RMW actually needs. `psumValid`
  // both advances the address and enables the write; `firstLoop` defeats the feedback
  // term so the slot is overwritten. `lastPsum` marks a PANEL boundary, which has no
  // meaning at slot granularity -- this level only restates its contract below.
  private val shiftEnable = io.accControl.psumValid
  private val sumEnable   = io.accControl.psumValid && !io.accControl.firstLoop

  // CONTRACT: `lastPsum` qualifies a psum cycle, so it is meaningless without
  // `psumValid`. Simulation-only ($error); the caller must gate it per bank.
  assert(
    !(io.accControl.lastPsum && !io.accControl.psumValid),
    "[error] AccSram: lastPsum asserted while psumValid is low",
    FAILURE
  )

  private def zeroInputWord = Vec(Seq.fill(numPorts)(portType.zeroSystolicOutputTypeC))
  private def zeroAccWord   = Vec(Seq.fill(numPorts)(portType.zeroAccumulatorOutputTypeC))

  // SRAM has no power-on value, and the first burst is only correct if a slot reads as
  // zero before it is first written. Today's control makes that true anyway (`firstLoop`
  // spans the whole first K-pass, overwriting every slot) and later bursts are cleared by
  // the drain's read-and-zero pass, so this init is a guard, not the load-bearing path:
  // it keeps simulation deterministic and holds the invariant if `firstLoop` is ever
  // narrowed. An ASIC macro has no init — add a reset-time clear or a warm-up drain over
  // all `depth` addresses.
  val mem = Mem(HardType(Vec(portType.createAccumulatorOutputTypeC, numPorts)), depth)
  mem.init(Seq.fill(depth)(zeroAccWord))

  // Free-running and never re-aligned from outside: callers keep it consistent by driving
  // whole address wraps. One malformed burst rotates every later panel (see the contract
  // checks in AccSramDoubleBuffer).
  val addr = Reg(UInt(log2Up(depth) bits)) init 0
  when(shiftEnable) {
    addr := Mux(addr === U(depth - 1), U(0, addr.getWidth bits), addr + 1)
  }

  // Synchronous read: data lands one cycle after the address is presented.
  val rdData = mem.readSync(addr)

  // Address, data and control re-timed by that one cycle so the modify-write half of the
  // RMW sees the operands the read was issued for.
  val addrD        = RegNext(addr)
  val sumEnableD   = RegNext(sumEnable, False)
  val shiftEnableD = RegNext(shiftEnable, False)
  val outputValidD = RegNext(io.outputValid, False)
  val inputD       = RegNext(io.input, zeroInputWord)

  val nextWord = Vec(portType.createAccumulatorOutputTypeC, numPorts)
  for (i <- 0 until numPorts) {
    // Array output is narrower than an accumulator slot; widen before adding.
    val widened  = inputD(i).resizeTo(rdData(i))
    val feedback = Mux(sumEnableD, rdData(i), portType.zeroAccumulatorOutputTypeC)
    nextWord(i) := feedback + widened
  }

  mem.write(address = addrD, data = nextWord, enable = shiftEnableD)

  // Holds the last captured word until the next `outputValid`, so a drain streams one slot
  // per cycle and an idle bank keeps its last value rather than flapping.
  io.output := RegNextWhen(
    next = rdData,
    cond = outputValidD,
    init = zeroAccWord
  )

}
