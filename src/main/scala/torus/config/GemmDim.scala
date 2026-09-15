package torus.config

import torus.core._

/**
 * One GEMM problem size: C[M×N] = A[M×K] · B[K×N].
 *
 * A pure PROBLEM description — it says nothing about the fabric that runs it, and the
 * same shape is swept across every config in a manifest. A (config, shape) pair that
 * does not fit is rejected by the config's own `require`s, never here.
 *
 *  - `name` is a human-readable label only. It is what verdict rows print, so it is the
 *    handle an operator uses to find a failing case.
 *  - `M` / `N` / `K` are the GEMM's own extents. They are NOT the array's provisioning
 *    caps — `freeDimensionSize` and `maxContractionSize` on [[SystolicArrayConfig]] are
 *    — and a shape smaller than those simply leaves hardware idle.
 *  - `schedule` is the optional explicit override; see [[scheduleOn]].
 */
final case class GemmDim(
  name:     String,
  M:        Int,
  N:        Int,
  K:        Int,
  schedule: Option[Schedule.Value] = None,
) {

  /**
   * The schedule this GEMM is driven through on `cfg`. Explicit wins; otherwise a
   * dual-mode fabric with M == 1 takes the GEMV path and everything else the stock
   * tiled one — the same routing the separate entry points performed before the
   * manifest collapsed them into one dispatch.
   *
   * Neither switch order ([[Schedule.SwitchGemmFirst]] / [[Schedule.SwitchGemvFirst]])
   * is ever derived — no function of (fabric, M) says the operator wanted a mode switch
   * — so they only ever arrive through the explicit field, either from a `"schedule"`
   * key in the shape file or from the case a manifest row names.
   */
  def scheduleOn(cfg: SystolicArrayConfig): Schedule.Value =
    schedule.getOrElse(
      if (cfg.isDualMode && M == 1) Schedule.Gemv else Schedule.Gemm
    )
}

/**
 * The shapes ONE dispatched row runs on.
 *
 * Every non-switch case uses `primary` alone and `secondary` is None. A switch case
 * needs two real shapes — one GEMM and one GEMV — so that the GEMV phase runs a shape
 * the operator NAMED in a file instead of one the runner invents for itself.
 *
 * `primary` is always the GEMM shape and `secondary` the GEMV shape, for BOTH switch
 * orders: the field says which SHAPE it is, not which phase runs first. Which one runs
 * first is the schedule's business.
 *
 * Built in `TestMain` from a manifest row's `"gemm"` / `"gemv"` file pair, and consumed
 * by `DataflowDriver.run`, which is where the two fields are finally told apart.
 */
final case class GemmSpec(primary: GemmDim, secondary: Option[GemmDim] = None) {
  /** Label for verdict rows: both shapes when there are two, so a switch case is
    * identifiable at a glance from the output alone. */
  def name: String = secondary match {
    case Some(g) => s"${primary.name} + ${g.name}"
    case None    => primary.name
  }
}
