package torus.config

import scala.io.Source

import torus.core._

/**
 * Loads SystolicArray generation configs from a JSON file.
 *
 * The JSON schema is:
 * {
 *   "configs": [
 *     {
 *       "name":        "<string>",
 *       "arch":        "Grouped" | "Toroidal" | "ToroidalDualMode",
 *       "numericType": "Signed" | "Unsigned",
 *       "dataflow":    "ReuseA" | "ReuseB" | "ReuseC",
 *       "dimensions":  { "outerPeRow": <int>, "outerPeCol": <int>,
 *                        "localVectorPeRow": <int>, "localVectorPeCol": <int>,
 *                        "multPerPe": <int> },
 *       "bitWidth":    { "inputA": <int>, "inputB": <int> },
 *       "accumulator": { "needAccumulator": <bool>,
 *                        "freeDimensionSize": <int>,   // required when needAccumulator
 *                        "maxContractionSize": <int>   // required when needAccumulator or ReuseC }
 *     }, ...
 *   ]
 * }
 *
 * Loading is in TWO stages and the split is deliberate:
 *
 *  1. [[GenConfig]] and the case classes above it mirror the JSON one-for-one. They
 *     hold raw strings and raw ints and assert NOTHING — `arch` and `dataflow` are
 *     still `String` at this point.
 *  2. [[toSystolicArrayConfig]] maps that onto the real [[SystolicArrayConfig]], where
 *     the constructor's `require` block is what finally judges it.
 *
 * So this file is a TRANSCRIBER, not a validator. The only errors it raises are ones
 * the config type could not raise for itself: an unknown enum spelling and an unknown
 * `arch`. Everything else — a dimension that is not a power of two, an accumulator
 * field declared by a dataflow that has no use for it — fails one step later, at
 * construction, with a message naming the field. Keeping it there means a config built
 * in Scala is checked exactly as strictly as one read from a file.
 *
 * Unknown keys are ignored (see [[JsonParser]]), which is what lets these files carry
 * `"_comment"` and `"description"` annotations.
 */

// ════════════════════════════════════════════════════════════
//  Case classes mirroring the JSON schema
// ════════════════════════════════════════════════════════════
// One per nested JSON object, with the file's own key names rather than the config's.
// They exist so a malformed file is a parse failure over a named field instead of a
// long positional argument list at the `SystolicArrayConfig` constructor.

/** The `"dimensions"` block. Both levels of the hierarchy: `outerPe*` counts OuterPEs
  * across the fabric, `localVectorPe*` counts VectorPEs inside one OuterPE, and
  * `multPerPe` counts multiplier lanes inside one VectorPE. */
final case class Dimensions(
  outerPeRow:       Int,
  outerPeCol:       Int,
  localVectorPeRow: Int,
  localVectorPeCol: Int,
  multPerPe:        Int,
)

/** The `"bitWidth"` block — operand widths only. Every accumulation width in the
  * design is derived from these by `PortTypeProvider` and is never declared. */
final case class BitWidth(
  inputA: Int,
  inputB: Int,
)

/**
 * The `"accumulator"` block.
 *
 * Both sizes default to 0 when the key is absent, and 0 means "not declared" rather
 * than "zero-sized": the config's `require` block reads it that way, demanding the
 * field where the hardware consumes it and REJECTING it where it does not. So an entry
 * with `"needAccumulator": false` and nothing else is complete, not incomplete.
 */
final case class AccumulatorSpec(
  needAccumulator:    Boolean,
  /** Extent of the free (non-contracted) output dimension held in the accumulator;
    * equals the AccSram depth. Absent (0) is only legal when `needAccumulator` is
    * false — ReuseC is output-stationary and has no external accumulator. */
  freeDimensionSize:  Int,
  /** Largest GEMM K this array is provisioned for; sizes the accumulator data width
    * (ReuseA/ReuseB) or the in-PE accumulation width (ReuseC). Absent (0) is only
    * legal when `needAccumulator` is false and the dataflow is not ReuseC. */
  maxContractionSize: Int,
)

/** One `"configs"` entry, verbatim. `arch`, `numericType` and `dataflow` are still
  * raw strings here — [[ConfigLoader.toSystolicArrayConfig]] is where they become the
  * types that carry meaning. `name` is a label only and reaches no hardware. */
final case class GenConfig(
  name:        String,
  arch:        String,
  numericType: String,
  dataflow:    String,
  dimensions:  Dimensions,
  bitWidth:    BitWidth,
  accumulator: AccumulatorSpec,
)

// ════════════════════════════════════════════════════════════
//  Loader
// ════════════════════════════════════════════════════════════

object ConfigLoader {

  import JsonParser._

  /** From `src/main/resources` (the normal path — manifests name resource-relative
    * files). */
  def loadFromResource(path: String): Seq[GenConfig] = {
    val src = Source.fromResource(path)
    val text =
      try src.mkString
      finally src.close()
    parseAll(text)
  }

  /** Load configs from a raw filesystem path, for a config outside the resource
    * tree. */
  def loadFromFile(path: String): Seq[GenConfig] = {
    val src = Source.fromFile(path)
    val text =
      try src.mkString
      finally src.close()
    parseAll(text)
  }

  private def parseAll(text: String): Seq[GenConfig] = {
    val root = asMap(JsonParser.parse(text))
    asList(root("configs")).map(e => parseGenConfig(asMap(e)))
  }

  private def parseDimensions(m: Map[String, Any]): Dimensions = Dimensions(
    outerPeRow       = getInt(m, "outerPeRow"),
    outerPeCol       = getInt(m, "outerPeCol"),
    localVectorPeRow = getInt(m, "localVectorPeRow"),
    localVectorPeCol = getInt(m, "localVectorPeCol"),
    multPerPe        = getInt(m, "multPerPe"),
  )

  private def parseBitWidth(m: Map[String, Any]): BitWidth = BitWidth(
    inputA = getInt(m, "inputA"),
    inputB = getInt(m, "inputB"),
  )

  // The only OPTIONAL fields in the schema: `getIntOpt` rather than `getInt`, so a
  // config that needs neither size can leave both keys out. Whether the 0 that results
  // is legal depends on the dataflow, and the config decides that, not this.
  private def parseAccumulator(m: Map[String, Any]): AccumulatorSpec = AccumulatorSpec(
    needAccumulator    = getBool(m, "needAccumulator"),
    freeDimensionSize  = getIntOpt(m, "freeDimensionSize").getOrElse(0),
    maxContractionSize = getIntOpt(m, "maxContractionSize").getOrElse(0),
  )

  private def parseGenConfig(m: Map[String, Any]): GenConfig = GenConfig(
    name        = getStr(m, "name"),
    arch        = getStr(m, "arch"),
    numericType = getStr(m, "numericType"),
    dataflow    = getStr(m, "dataflow"),
    dimensions  = parseDimensions(asMap(m("dimensions"))),
    bitWidth    = parseBitWidth(asMap(m("bitWidth"))),
    accumulator = parseAccumulator(asMap(m("accumulator"))),
  )

  // String -> enum, for the two fields the JSON spells out in words. Exhaustive with a
  // failing default: an unrecognised spelling names both what was read and what was
  // expected, because a typo here would otherwise pick a silently different machine.
  private def toNumericType(s: String): NumericType = s match {
    case "Signed"   => NumericType.Signed
    case "Unsigned" => NumericType.Unsigned
    case other      => sys.error(s"Unknown numericType: $other (expected Signed | Unsigned)")
  }

  private def toDataflow(s: String): Dataflow.Value = s match {
    case "ReuseA" => Dataflow.ReuseA
    case "ReuseB" => Dataflow.ReuseB
    case "ReuseC" => Dataflow.ReuseC
    case other    => sys.error(s"Unknown dataflow: $other (expected ReuseA | ReuseB | ReuseC)")
  }

  /**
   * The stage-2 mapping: a transcribed [[GenConfig]] becomes the real config, and its
   * constructor's `require` block runs.
   *
   * `arch` picks the CLASS, and the class is the only thing that differs — the three
   * branches pass identical arguments. That repetition is the price of the configs
   * being separate case classes, which is in turn what makes the fabric dispatch
   * (`ArrayTopology`, `SystolicArray.buildDefinitionName`, `DataflowDriver.run`) a
   * match on a sealed type rather than on a string.
   */
  def toSystolicArrayConfig(g: GenConfig): SystolicArrayConfig = g.arch match {
    case "Grouped" =>
      GroupedConfig(
        numericType              = toNumericType(g.numericType),
        dataflow                 = toDataflow(g.dataflow),
        outerPeRow               = g.dimensions.outerPeRow,
        outerPeCol               = g.dimensions.outerPeCol,
        localVectorPeRow         = g.dimensions.localVectorPeRow,
        localVectorPeCol         = g.dimensions.localVectorPeCol,
        multPerPe                = g.dimensions.multPerPe,
        bitWidthA                = g.bitWidth.inputA,
        bitWidthB                = g.bitWidth.inputB,
        needAccumulator          = g.accumulator.needAccumulator,
        freeDimensionSize        = g.accumulator.freeDimensionSize,
        maxContractionSize       = g.accumulator.maxContractionSize,
      )

    case "Toroidal" =>
      ToroidalConfig(
        numericType              = toNumericType(g.numericType),
        dataflow                 = toDataflow(g.dataflow),
        outerPeRow               = g.dimensions.outerPeRow,
        outerPeCol               = g.dimensions.outerPeCol,
        localVectorPeRow         = g.dimensions.localVectorPeRow,
        localVectorPeCol         = g.dimensions.localVectorPeCol,
        multPerPe                = g.dimensions.multPerPe,
        bitWidthA                = g.bitWidth.inputA,
        bitWidthB                = g.bitWidth.inputB,
        needAccumulator          = g.accumulator.needAccumulator,
        freeDimensionSize        = g.accumulator.freeDimensionSize,
        maxContractionSize       = g.accumulator.maxContractionSize,
      )

    case "ToroidalDualMode" =>
      // Expanded Toroidal with a time-shared second input stream. ReuseA / ReuseB
      // only — the config itself rejects ReuseC.
      ToroidalDualModeConfig(
        numericType              = toNumericType(g.numericType),
        dataflow                 = toDataflow(g.dataflow),
        outerPeRow               = g.dimensions.outerPeRow,
        outerPeCol               = g.dimensions.outerPeCol,
        localVectorPeRow         = g.dimensions.localVectorPeRow,
        localVectorPeCol         = g.dimensions.localVectorPeCol,
        multPerPe                = g.dimensions.multPerPe,
        bitWidthA                = g.bitWidth.inputA,
        bitWidthB                = g.bitWidth.inputB,
        needAccumulator          = g.accumulator.needAccumulator,
        freeDimensionSize        = g.accumulator.freeDimensionSize,
        maxContractionSize       = g.accumulator.maxContractionSize,
      )

    case other =>
      sys.error(s"[${g.name}] Unknown arch: $other (expected Grouped | Toroidal | ToroidalDualMode)")
  }
}
