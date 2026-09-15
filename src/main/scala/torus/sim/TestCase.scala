package torus.sim

import torus.core._

/**
 * One named row of the simulation coverage matrix.
 *
 * The key is `(fabric, schedule, acc)`, which is the dispatch key
 * [[DataflowDriver.run]] already used; naming it is what lets a case be RUN by name,
 * FILTERED by name and REPORTED by name. Before this existed, a combination with no
 * coverage at all and a combination that was never asked for printed the same thing:
 * nothing.
 *
 * `dataflow` is deliberately NOT part of the key. A case spans the dataflows its fabric
 * supports (ReuseA/ReuseB/ReuseC for the no-accumulator rows, ReuseA/ReuseB for the
 * rest), the runners treat the mirror pair identically, and which dataflow a config uses
 * is already in the config's own name.
 *
 * `claim` is one line saying what a PASS here proves. It is printed in the matrix
 * summary, so a reader does not have to open five scaladoc blocks to learn what the
 * suite is asserting.
 */
sealed abstract class TestCase(
  val id:       String,
  val fabric:   Fabric.Value,
  val schedule: Schedule.Value,
  val acc:      Boolean,
  val claim:    String,
)

object TestCase {

  // ── Grouped ────────────────────────────────────────────────────────────────
  case object GroupedNoAcc extends TestCase("grouped-noacc", Fabric.Grouped, Schedule.Gemm, false,
    "the Grouped fabric's ReuseA/ReuseB/ReuseC per-tile partial sums are exact, and their " +
      "k-tiles sum to C")
  case object GroupedAcc extends TestCase("grouped-acc", Fabric.Grouped, Schedule.Gemm, true,
    "the Grouped de-skew + AccTap path drains a whole accumulator panel exactly, ReuseA/ReuseB " +
      "(ReuseC has no AccSram)")

  // ── Toroidal ───────────────────────────────────────────────────────────────
  case object ToroidalNoAcc extends TestCase("toroidal-noacc", Fabric.Toroidal, Schedule.Gemm, false,
    "the Toroidal fabric's ReuseA/ReuseB/ReuseC per-tile partial sums are exact, and their " +
      "k-tiles sum to C")
  case object ToroidalAcc extends TestCase("toroidal-acc", Fabric.Toroidal, Schedule.Gemm, true,
    "the Toroidal fabric drains a whole accumulator panel exactly, ReuseA/ReuseB " +
      "(ReuseC has no AccSram)")

  // ── The dual-mode wrapper, stock tiled GEMM ────────────────────────────────
  case object DualGemmNoAcc extends TestCase("dual-gemm-noacc", Fabric.ToroidalDual, Schedule.Gemm, false,
    "with both mode selects low the wrapper IS the Toroidal array it is built around")
  case object DualGemmAcc extends TestCase("dual-gemm-acc", Fabric.ToroidalDual, Schedule.Gemm, true,
    "the wrapper's accumulator panel schedule matches the unwrapped array's")

  // ── The dual-mode wrapper, low-M GEMV ──────────────────────────────────────
  case object DualGemvNoAcc extends TestCase("dual-gemv-noacc", Fabric.ToroidalDual, Schedule.Gemv, false,
    "the GEMV broadcast + second-stream path is exact per K-tile, and the per-K-tile software " +
      "summation reaches C")
  case object DualGemvAcc extends TestCase("dual-gemv-acc", Fabric.ToroidalDual, Schedule.Gemv, true,
    "the GEMV schedule accumulates K-passes in the AccSram and drains a panel exactly")

  // ── The runtime mode switch, both orders, with and without the accumulator ──
  case object DualSwitchGemm extends TestCase("dual-switch-gemm", Fabric.ToroidalDual, Schedule.SwitchGemmFirst, false,
    "Gemm → Gemv → Gemm on one elaborated array with one reset: the GEMV burst leaves no state " +
      "the following GEMM can see")
  case object DualSwitchGemmAcc extends TestCase("dual-switch-gemm-acc", Fabric.ToroidalDual, Schedule.SwitchGemmFirst, true,
    "the same, with the accumulator attached: a GEMV panel and a GEMM panel share one address " +
      "space and each bank returns to its starting rotation")
  case object DualSwitchGemv extends TestCase("dual-switch-gemv", Fabric.ToroidalDual, Schedule.SwitchGemvFirst, false,
    "the mirror order, Gemv → Gemm → Gemv: a GEMM tile leaves no residue in the second-stream " +
      "registers the following GEMV can see")
  case object DualSwitchGemvAcc extends TestCase("dual-switch-gemv-acc", Fabric.ToroidalDual, Schedule.SwitchGemvFirst, true,
    "the mirror order with the accumulator attached")

  /**
   * Every case, in the order the matrix summary prints them.
   *
   * `dual-gemv-noacc` and `dual-gemm-acc` are ordinary rows, not an afterthought: they
   * are the only coverage of the GEMV path's per-K-tile software summation and of the
   * wrapper's accumulator panel schedule, and they are the two halves the switch cases
   * switch BETWEEN, so a failing switch can be bisected against them.
   */
  val all: Seq[TestCase] = Seq(
    GroupedNoAcc, GroupedAcc,
    ToroidalNoAcc, ToroidalAcc,
    DualGemmNoAcc, DualGemmAcc,
    DualGemvNoAcc, DualGemvAcc,
    DualSwitchGemm, DualSwitchGemmAcc,
    DualSwitchGemv, DualSwitchGemvAcc,
  )

  /**
   * The case a (fabric, schedule, acc) triple belongs to, or None if the combination has
   * no runner. The key is unchanged from what `run` already matched on — this only gives
   * it a name.
   */
  def of(fabric: Fabric.Value, schedule: Schedule.Value, acc: Boolean): Option[TestCase] =
    all.find(c => c.fabric == fabric && c.schedule == schedule && c.acc == acc)

  def byId(id: String): Option[TestCase] = all.find(_.id == id)

  /** Parse a `--case a,b,c` filter value into the cases it names. */
  def parseFilter(spec: String): Seq[TestCase] =
    spec.split(",").iterator.map(_.trim).filter(_.nonEmpty).map { id =>
      byId(id).getOrElse(sys.error(
        s"[error] unknown case id '$id' (known: ${all.map(_.id).mkString(", ")})"))
    }.toSeq
}

/**
 * What one (config, GEMM) row of a run came to.
 *
 * A `Boolean` conflated "skipped" with "passed" — every skip in the old dispatcher
 * returned true — so a run consisting entirely of skips was indistinguishable from a run
 * that proved something. Making the third outcome a value of its own is what lets the
 * matrix summary report coverage rather than just verdicts.
 */
sealed trait Verdict {
  def isFail: Boolean = this.isInstanceOf[Verdict.Fail]
  def isPass: Boolean = this == Verdict.Pass
  def isSkip: Boolean = this.isInstanceOf[Verdict.Skip]
}

object Verdict {
  case object Pass extends Verdict
  final case class Fail(why: String) extends Verdict
  final case class Skip(why: String) extends Verdict
}

/**
 * One row of the run: which case it covered (None when the combination has no runner at
 * all), the label it printed under, and its verdict.
 */
final case class CaseResult(testCase: Option[TestCase], label: String, verdict: Verdict)
