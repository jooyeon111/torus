package torus.config

import scala.io.Source

import torus.core._

/**
 * Loads a list of GEMM problem sizes ([[GemmDim]]) from a JSON file.
 *
 * Schema:
 * {
 *   "gemms": [
 *     { "name": "GEMM_tiny",  "M": 4,  "N": 4,  "K": 8  },
 *     { "name": "GEMM_small", "M": 8,  "N": 8,  "K": 16 },
 *     { "name": "SWITCH_16",  "M": 16, "N": 16, "K": 16, "schedule": "switch" },
 *     ...
 *   ]
 * }
 *
 * `"schedule"` is OPTIONAL ("gemm" | "gemv" | "switch" | "switch_gemm" |
 * "switch_gemv"). Absent, the schedule is derived from the (fabric, M) pair — see
 * [[GemmDim.scheduleOn]].
 *
 * Unknown keys are IGNORED, which is what lets the shape files carry `"_comment"` /
 * `"description"` annotations without the parser knowing about them.
 *
 * A file normally holds several shapes and a suite sweeps every (config × GEMM)
 * combination; the one exception is a mode-switch pair, where `TestMain` requires
 * exactly one entry per file so the two phases are unambiguous.
 *
 * Reuses the same [[JsonParser]] as [[ConfigLoader]] so both file families share one
 * parser implementation.
 */
object GemmLoader {

  import JsonParser._

  /** From `src/main/resources` (the normal path — manifests name resource-relative
    * files). */
  def loadFromResource(path: String): Seq[GemmDim] = {
    val src = Source.fromResource(path)
    val text =
      try src.mkString
      finally src.close()
    parseAll(text)
  }

  /** From a raw filesystem path, for a shape file outside the resource tree. */
  def loadFromFile(path: String): Seq[GemmDim] = {
    val src = Source.fromFile(path)
    val text =
      try src.mkString
      finally src.close()
    parseAll(text)
  }

  private def parseAll(text: String): Seq[GemmDim] = {
    val root = asMap(JsonParser.parse(text))
    asList(root("gemms")).map(e => parseGemm(asMap(e)))
  }

  private def parseGemm(m: Map[String, Any]): GemmDim = GemmDim(
    name     = getStr(m, "name"),
    M        = getInt(m, "M"),
    N        = getInt(m, "N"),
    K        = getInt(m, "K"),
    // Absent -> None -> the schedule is derived from (fabric, M). Only a sequence the
    // derivation cannot reach (Switch) or a deliberate off-derivation regression target
    // needs to spell it out.
    schedule = getStrOpt(m, "schedule").map(Schedule.parse),
  )
}
