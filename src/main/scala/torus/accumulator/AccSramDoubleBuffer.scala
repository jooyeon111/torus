package torus.accumulator

import spinal.core._

import torus.arithmetic._
import torus.config._

/**
 * Two BIG synchronous SRAMs in ping-pong, accumulating the partial sums of ALL
 * output ports together.
 *
 * There are exactly TWO memories total — each a wide [[AccSram]] bank holding every
 * port — so a `systolicOutputDimC`-wide array uses 2 macros instead of one pair of
 * small SRAMs per output port. `depth` is output-indexed (see [[AccSram]]): a whole
 * panel of partial sums stays resident and drains as one burst.
 *
 * This module is the CONTROL layer over those two banks. It does one thing: detect the
 * end of an output panel and swap the banks' roles, so the finished panel drains while
 * the next one is already accumulating. Everything else is wiring that translates a
 * bank's role into a bank-local [[AccControl]] plus a read strobe. For the shape of the
 * three control fields — `firstLoop` is a level, `lastPsum` a pulse — see [[AccControl]].
 *
 * ==True ping-pong==
 * Two INDEPENDENT selects: `writeSel` is the bank accumulating psums, `drainSel` the
 * bank being drained. On `burstEnded` the just-filled bank is handed to the drain path
 * and the write select flips, so the next panel starts at once instead of waiting the
 * drain out. A single select serving both roles (the pre-fix design) routed the new
 * panel's psums back into the still-draining bank and forced a full drain gap between
 * panels.
 *
 * ==Caller contract==
 * Every `psumValid` run must be a whole number of `depth`-cycle address wraps, and
 * `lastPsum` must land on the run's final cycle. Nothing in the datapath can detect a
 * violation: each bank's `addr` free-runs and is never re-aligned, so ONE malformed
 * burst permanently rotates the slot-to-output mapping and every later panel lands on
 * the wrong addresses — no X, no stall, just wrong numbers. The asserts below exist to
 * turn that silence into a loud failure; they are simulation-only ($error) and cost no
 * gates once stripped for synthesis.
 */
class AccSramDoubleBuffer[InputType <: Data, AccType <: Data](
  config: SystolicArrayConfig,
  numPorts: Int,
)(
  implicit val arithmetic: Arithmetic[InputType, AccType],
  implicit val portType: PortTypeProvider[InputType, AccType],
) extends Component {

  // Output-indexed (see [[AccSram]]): the free output dimension, so a whole panel of
  // partial sums stays resident and drains as one ping-pong burst.
  val depth = config.freeDimensionSize

  val io = new Bundle {
    val input = in Vec(portType.createSystolicOutputTypeC, numPorts)

    val accControl = in (AccControl())

    val output = out Vec(portType.createAccumulatorOutputTypeC, numPorts)
  }

  private def zeroInputWord = Vec(Seq.fill(numPorts)(portType.zeroSystolicOutputTypeC))

  // The drain reuses the accumulate datapath instead of a second port: input forced to
  // zero and `sumEnable` low writes `0 + 0` back one cycle after each read, while
  // `shiftEnable` stays high for exactly one address wrap — so a slot is read out and
  // cleared in the same pass. The wrap needs `depth - 1` counted cycles on top of the
  // `burstEnded` cycle; `depth` keeps one cycle of slack for the sync read pipeline.
  // Anything longer only re-reads already-zeroed slots. (Measured: `depth - 2` fails.)
  val drainLen = depth
  val drainCounter = Reg(UInt(log2Up(drainLen + 1) bits)) init (0)
  val draining     = drainCounter =/= 0

  // Panel boundary: the FALLING edge of `psumValid`, qualified by `lastPsum` so a mid-panel
  // bubble is not mistaken for the end. One-cycle pulse, and the sole trigger of every
  // state transition in this module.
  val psumValidPrev = RegNext(io.accControl.psumValid, False)
  val lastPsumPrev  = RegNext(io.accControl.lastPsum, False)
  val burstEnded    = psumValidPrev && lastPsumPrev && !io.accControl.psumValid

  when(burstEnded) {
    drainCounter := drainLen
  } elsewhen (draining) {
    drainCounter := drainCounter - 1
  }

  // CONTRACT: every psumValid run is a whole number of address wraps. The counter is
  // modulo `depth`, so it lands back on 0 exactly when the run was a multiple of `depth`;
  // the clear takes effect a cycle later, so the falling-edge cycle still reads the
  // end-of-run value.
  val psumRunCounter = Reg(UInt(log2Up(depth) bits)) init 0
  when(io.accControl.psumValid) {
    psumRunCounter := Mux(psumRunCounter === U(depth - 1), U(0, psumRunCounter.getWidth bits), psumRunCounter + 1)
  } otherwise {
    psumRunCounter := 0
  }
  assert(
    !(psumValidPrev && !io.accControl.psumValid && psumRunCounter =/= 0),
    s"[error] AccSramDoubleBuffer: a psumValid burst was not a multiple of depth ($depth) — every AccSram addr is now permanently misaligned",
    FAILURE
  )

  // CONTRACT: `lastPsum` marks a psum cycle, so it is meaningless without `psumValid` — a
  // stray pulse is silently dropped instead of arming the drain, and the panel then runs
  // on into the next one.
  assert(
    !(io.accControl.lastPsum && !io.accControl.psumValid),
    "[error] AccSramDoubleBuffer: lastPsum asserted while psumValid is low",
    FAILURE
  )

  // BACKSTOP: a panel ending mid-drain would reload `drainCounter` and re-point
  // `drainSelReg`, truncating the drain and leaving stale psums behind. Unreachable while
  // the burst-length check holds — a collision implies a panel shorter than `depth`, which
  // that check reports first with a better message.
  assert(
    !(burstEnded && draining),
    "[error] AccSramDoubleBuffer: a burst ended while the previous drain was still running",
    FAILURE
  )

  // `writeSel` = the bank accumulating psums; `drainSel` = the bank being drained.
  val writeSelReg = RegInit(False)
  val drainSelReg = RegInit(False)

  // Hand the just-filled bank to the drain path and flip the write select, so the NEXT
  // panel's psums target the other bank immediately. This is what removes the mandatory
  // per-panel drain gap.
  when(burstEnded) {
    drainSelReg := writeSelReg
    writeSelReg := !writeSelReg
  }

  // The flip above is only effective NEXT cycle, so on the `burstEnded` cycle itself the
  // drain must still point at the bank that was being written — the pre-flip value.
  val writeSel = writeSelReg
  val drainSel = Mux(burstEnded, writeSelReg, drainSelReg)

  val drainActive = burstEnded || draining

  val buffer0 = new AccSram(config, numPorts)
  val buffer1 = new AccSram(config, numPorts)

  // Translates a bank's role into the [[AccControl]] that bank should see. A bank writes
  // when `writeSel` picks it and a psum is valid, and drains when `drainSel` picks it and
  // the drain is active. Each bank's free-running `addr` needs no re-alignment: a panel
  // accumulates over a whole multiple of `depth` cycles, so the drain reads from the
  // rotational offset the accumulate started at.
  def driveBank(bank: AccSram[InputType, AccType], isWrite: Bool, isDrain: Bool): Unit = {
    val writing      = isWrite && io.accControl.psumValid
    val drainingBank = isDrain && drainActive
    // BACKSTOP for the invariant the two roles lean on below: a bank is picked by BOTH
    // selects only on `burstEnded`, where psumValid — and so `writing` — is low. That is why
    // the write terms can be gated on `isWrite` alone rather than on `!drainingBank`; were
    // they ever to overlap, the drain's zero input and `firstLoop` would win and wipe the
    // panel instead of accumulating it. Collapse the two selects back into one and this is
    // the assert that fires.
    assert(
      !(writing && drainingBank),
      "[error] AccSramDoubleBuffer: a bank is accumulating and draining on the same cycle",
      FAILURE
    )
    // The bank's own view of [[AccControl]]. A drain needs no extra port: it IS a psum
    // pass that overwrites every slot with zero, so `firstLoop` goes high alongside the
    // zeroed input and the accumulate feedback drops out on its own. `lastPsum` is
    // gated by `writing` so it never outlives this bank's `psumValid` (AccSram asserts
    // on that) — the drained bank's panel boundary already lives in `burstEnded` here.
    bank.io.input                := Mux(drainingBank, zeroInputWord, io.input)
    bank.io.accControl.psumValid := writing || drainingBank
    bank.io.accControl.firstLoop := drainingBank || (isWrite && io.accControl.firstLoop)
    bank.io.accControl.lastPsum  := writing && io.accControl.lastPsum
    bank.io.outputValid          := drainingBank
  }

  driveBank(buffer0, writeSel === False, drainSel === False)
  driveBank(buffer1, writeSel === True,  drainSel === True)

  val bankOutputs = Vec(buffer0.io.output, buffer1.io.output)
  io.output := bankOutputs(drainSel.asUInt)

}
