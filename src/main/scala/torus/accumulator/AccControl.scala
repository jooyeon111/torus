package torus.accumulator

import spinal.core._

/**
 * The accumulator control triple, shared by every output port.
 *
 * One copy drives all ports of an [[AccSramDoubleBuffer]] bank, so it is valid only
 * where the ports are aligned: the Grouped ReuseA / ReuseB paths (de-skewed by the
 * TypeC buffer and re-timed by [[AccTap]]) and the Toroidal ReuseB path.
 *
 * The three fields are not symmetric in SHAPE, which is the usual source of confusion:
 * {{{
 *             ├─ K-pass 0 ─┤├─ K-pass 1 ─┤   (each exactly `freeDimensionSize` cycles)
 *   psumValid ████████████████████████████____
 *   firstLoop ██████████████__________________   level: the whole first K-pass
 *   lastPsum  ___________________________█____   pulse: the final psum cycle only
 * }}}
 *  - `psumValid` — this cycle's psum is valid; its span is one output panel.
 *  - `firstLoop` — first K-pass, so OVERWRITE the slot instead of accumulating.
 *  - `lastPsum`  — the panel's last psum cycle. It does not itself start the drain;
 *                  it qualifies the `psumValid` falling edge that does.
 */
case class AccControl() extends Bundle {
  val psumValid = Bool()
  val firstLoop = Bool()
  val lastPsum = Bool()
}

object AccControl {
  def apply(firstLoop: Bool, lastPsum: Bool, psumValid: Bool): AccControl = {
    val ctrl = AccControl()
    ctrl.firstLoop := firstLoop
    ctrl.lastPsum  := lastPsum
    ctrl.psumValid := psumValid
    ctrl
  }

  // Reset / `Delay` init value: all-low is the only safe idle, since a stray psumValid
  // would advance an AccSram address and desync it for the rest of the run.
  def zero: AccControl = {
    val ctrl = AccControl()
    ctrl.firstLoop := False
    ctrl.lastPsum  := False
    ctrl.psumValid := False
    ctrl
  }
}
