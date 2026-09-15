package torus.core

/**
 * Which schedule a (config, GEMM) pair is driven through.
 *
 * The schedule is the second half of the simulation harness's dispatch key (the first
 * being [[Fabric]]); together with `dataflow` and `needAccumulator` they select exactly
 * one runner in `DataflowDriver.run`.
 */
object Schedule extends Enumeration {
  /** Stock tiled GEMM: C[M×N] = A[M×K]·B[K×N], both dual-mode selects low. */
  val Gemm = Value
  /** Low-M GEMV: C[1×N] = A[1×K]·B[K×N], stationary broadcast + per-VectorPE lanes. */
  val Gemv = Value
  /** Gemm → Gemv → Gemm on ONE elaborated array with ONE reset. Never derived;
    * only ever requested explicitly through `GemmDim.schedule`. */
  val SwitchGemmFirst = Value
  /**
   * Gemv → Gemm → Gemv, the mirror.
   *
   * Worth its own value because the residue claim is DIRECTIONAL. [[SwitchGemmFirst]]
   * proves a GEMV burst leaves no state the following GEMM can see; it does not prove
   * the converse, and the two use different halves of the fabric — the GEMM phase writes
   * every group of the stationary bus and streams the systolic edge, the GEMV phase
   * writes group 0 and lets `StationaryDemux` broadcast it. Residue from a GEMM tile
   * sitting in the second-stream registers is exactly what the first order cannot see.
   */
  val SwitchGemvFirst = Value

  /** Every schedule that is a mode switch, whichever mode it starts in. */
  def isSwitch(v: Value): Boolean = v == SwitchGemmFirst || v == SwitchGemvFirst

  /** The mode a switch schedule's first (and third) phase runs in. */
  def firstPhaseOf(v: Value): Value = v match {
    case SwitchGemmFirst => Gemm
    case SwitchGemvFirst => Gemv
    case other           => other
  }

  /** Parse the optional `"schedule"` key of a GEMM JSON entry. */
  def parse(s: String): Value = s.toLowerCase match {
    case "gemm"                   => Gemm
    case "gemv"                   => Gemv
    // "switch" keeps its meaning, so gemm_dual_switch.json routes unchanged.
    case "switch" | "switch_gemm" => SwitchGemmFirst
    case "switch_gemv"            => SwitchGemvFirst
    case other    => sys.error(
      s"[error] Unknown schedule '$other' (expected gemm | gemv | switch | switch_gemm | switch_gemv)")
  }
}

/**
 * Which fabric the config elaborates.
 *
 * Derived from the config's CLASS, so it is the one axis `DataflowDriver.run` could
 * not read before: `ToroidalDualModeConfig` extends `SystolicArrayConfig` directly, so
 * a dual config answers false to BOTH `isGrouped` and `isToroidal`.
 */
object Fabric extends Enumeration {
  val Grouped, Toroidal, ToroidalDual = Value
}
