package torus

import spinal.core._
import spinal.core.SpinalConfig

import java.io.File

import torus.arithmetic._
import torus.array._
import torus.config._
import torus.core._

/**
 * Main entry point for generating Systolic Array RTL from a JSON config file.
 *
 * Usage:
 *   sbt "runMain torus.GenerationMain [configJson] [outputDir]"
 *
 *
 * The config file is loaded as a classpath resource. If the argument
 * already contains a '/', it is used as-is; otherwise `gen_config/` is
 * prepended automatically so callers can pass just the filename.
 *
 * JSON parsing, schema definition, and the GenConfig -> SystolicArrayConfig
 * conversion all live in `torus.config` so they can be shared with test code.
 *
 * One config file may hold SEVERAL entries and each is generated independently: a throw
 * becomes a `FAIL` line and the sweep continues, so one broken entry cannot cost the rest
 * of the file. The summary at the end is the authoritative result, and a non-zero exit
 * follows from it — which is what makes `make gen` usable in CI.
 *
 * The sibling entry point is [[TestMain]], which drives the same configs in simulation
 * rather than emitting RTL.
 */
object GenerationMain {

  private def resolve(arg: String, dir: String): String =
    if (arg.contains('/')) arg else s"$dir/$arg"

  // Dual mode is a FLAG on the Toroidal array, not an array of its own: the fabric it
  // expands is the plain Toroidal one, so it is built from the equivalent
  // [[ToroidalConfig]] (`toToroidalConfig`) with `dualMode = true`.
  private def buildTopComponent(cfg: SystolicArrayConfig): Component = cfg.numericType match {
    case NumericType.Signed =>
      implicit val arith: Arithmetic[SInt, SInt] = Arithmetic.sIntArithmetic
      implicit val portType: PortTypeProvider[SInt, SInt] = new SignedPortTypeProvider(cfg)
      cfg match {
        case _: GroupedConfig          => new GroupedSystolicArray[SInt, SInt](cfg)
        case _: ToroidalConfig         => new ToroidalSystolicArray[SInt, SInt](cfg)
        case c: ToroidalDualModeConfig => new ToroidalSystolicArray[SInt, SInt](c.toToroidalConfig, dualMode = true)
      }

    case NumericType.Unsigned =>
      implicit val arith: Arithmetic[UInt, UInt] = Arithmetic.uIntArithmetic
      implicit val portType: PortTypeProvider[UInt, UInt] = new UnsignedPortTypeProvider(cfg)
      cfg match {
        case _: GroupedConfig          => new GroupedSystolicArray[UInt, UInt](cfg)
        case _: ToroidalConfig         => new ToroidalSystolicArray[UInt, UInt](cfg)
        case c: ToroidalDualModeConfig => new ToroidalSystolicArray[UInt, UInt](c.toToroidalConfig, dualMode = true)
      }
  }

  private def generateOne(g: GenConfig, baseOutputDir: String): Unit = {
    println(s"\n[gen] ── ${g.name} ──")
    println(s"      arch=${g.arch}  numeric=${g.numericType}  dataflow=${g.dataflow}")

    // Flat output: every entry writes into the same directory rather than a per-entry
    // subdirectory (the commented form). Entry names are unique within a file, so the
    // products do not collide, and a flat tree is what the Makefile's RTL_DIR expects.
    // val targetDir = new File(baseOutputDir, g.name)
    val targetDir = new File(baseOutputDir)
    targetDir.mkdirs()

    val cfg = ConfigLoader.toSystolicArrayConfig(g)

    val spinalConfig = SpinalConfig(
      targetDirectory             = targetDir.getPath,
      defaultClockDomainFrequency = FixedFrequency(1 GHz),
      oneFilePerComponent = false,
    )

    // The array names itself from its own shape (`SInt_ReuseB_Toroidal_...`, see
    // SystolicArray), which is right for identifying a netlist but not for a file the
    // config ASKED for by name. Overriding with `g.name` is what makes the emitted module
    // match the entry the operator wrote, so `oneFilePerComponent = false` lands
    // everything in one predictable `<name>.v`.
    val spinalReport = spinalConfig.generateSystemVerilog {
      val top = buildTopComponent(cfg)
      top.setDefinitionName(g.name)
      top
    }

    // Pruned-signal reports are printed per entry, not summarised: pruned IO in
    // particular usually means a port the topology generated and nothing drove.
    println(s"      → ${targetDir.getPath}/${g.name}.v")
    println(s"\n[prune] ── ${g.name} pruned signals ──")
    spinalReport.printPruned()
    println(s"\n[prune] ── ${g.name} pruned IO only ──")
    spinalReport.printPrunedIo()
  }

  def main(args: Array[String]): Unit = {
    val configArg = args.lift(0).getOrElse("grouped_toroidal_scalar_systolic_array.json")
    val outputDir = args.lift(1).getOrElse("output")

    val configPath = resolve(configArg, "gen_config")

    println(s"[Main] config file : $configPath")
    println(s"[Main] output dir  : $outputDir")

    val configs = ConfigLoader.loadFromResource(configPath)
    println(s"[Main] loaded ${configs.size} configuration(s)")

    val results = configs.map { g =>
      val ok =
        try {
          generateOne(g, outputDir)
          true
        } catch {
          case e: Throwable =>
            println(s"[gen] FAILED ${g.name}: ${e.getMessage}")
            e.printStackTrace()
            false
        }
      g.name -> ok
    }

    println("\n[Main] ── summary ──")
    results.foreach { case (n, ok) =>
      println(f"  ${if (ok) "OK   " else "FAIL "}  $n")
    }

    val failed = results.count { case (_, ok) => !ok }
    if (failed > 0) sys.exit(1)
  }

}