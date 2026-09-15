package torus

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import torus.config._
import torus.sim._

/**
 * The simulation harness's single entry point.
 *
 * Usage:
 *   sbt "runMain torus.TestMain [configJson] [gemmJson]"
 *   sbt "runMain torus.TestMain --suite <suiteJson>"
 *   sbt "runMain torus.TestMain --suite <suiteJson> --case <id>[,<id>…]"
 *
 * End-to-end only: hand every (config, GEMM) pair to [[DataflowDriver.run]], which keys
 * on `(fabric, schedule, dataflow, needAccumulator)`. The fabric comes off the config's
 * class and the schedule off the GEMM, so Grouped, Toroidal and ToroidalDualMode arrays
 * — and the stock tiled-GEMM, low-M GEMV and mode-switch schedules — are all reachable
 * from this one main.
 *
 * NOTHING is pre-filtered here except `--case`. The dispatcher already knows which
 * combinations it can drive, so an unsupported pair gets one consistent `[skip]` line
 * with a reason from `run` instead of a differently-worded one from each main.
 *
 * The run ends in the COVERAGE MATRIX rather than a flat verdict list: one line per
 * named [[TestCase]], including the cases with no rows at all. That last part is the
 * point — a case with zero coverage used to be indistinguishable from a case nobody
 * asked for, which is exactly how two config files sat unreachable from `make` until the
 * previous migration found them.
 *
 * The block-level [[AccSramDoubleBuffer]] test that used to run here as a first
 * `--block` stage now lives in [[AccSramDoubleBufferTest]] (scenario D).
 */
object TestMain {

  private def resolve(arg: String, dir: String): String =
    if (arg.contains('/')) arg else s"$dir/$arg"

  /**
   * One row of a run: a config file, a GEMM shape file, and optionally the GEMV shape
   * file a mode-switch case needs as its second phase, plus optionally the [[TestCase]]
   * the row asserts it covers.
   *
   * `kase` does two things, and both matter because a config FILE holds several fabrics:
   *
   *  - it SELECTS. Only the config entries that resolve to that case run, so
   *    `1.conventional_systolic_array.json` serves the Grouped row and the Toroidal row of
   *    a manifest without either duplicating a sim.
   *  - it names the SCHEDULE. The switch shape files are ordinary shapes reusable by other
   *    cases and carry no `"schedule"` of their own, so it is the ROW that says a switch is
   *    being run.
   *
   * A row that names a case and then matches NO config entry is a manifest error — that is
   * the check against the matrix, and it is what catches a typo'd id or a row pointed at
   * the wrong config file.
   */
  private final case class Pair(
    config: String,
    gemm:   String,
    gemv:   Option[String] = None,
    kase:   Option[TestCase] = None,
  )

  /**
   * A suite manifest: a named list of (config, GEMM) file pairs.
   *
   * {
   *   "name": "dual",
   *   "runs": [ { "config": "5.toroidal_dual_mode.json", "gemm": "gemm_dual_m1.json" }, ... ]
   * }
   *
   * The three entry points this one replaces each carried a different default pair and
   * the Makefile invoked six distinct ones, so the suite file is where a multi-pair run
   * is now declared instead of in a `main`.
   */
  private def loadSuite(path: String): (String, Seq[Pair]) = {
    val src  = Source.fromResource(path)
    val text = try src.mkString finally src.close()
    val root = JsonParser.asMap(JsonParser.parse(text))
    val name = JsonParser.getStrOpt(root, "name").getOrElse(path)
    val runs = JsonParser.asList(root("runs")).map { e =>
      val m = JsonParser.asMap(e)
      Pair(
        config = JsonParser.getStr(m, "config"),
        gemm   = JsonParser.getStr(m, "gemm"),
        gemv   = JsonParser.getStrOpt(m, "gemv"),
        kase   = JsonParser.getStrOpt(m, "case").map(id => TestCase.byId(id).getOrElse(
          sys.error(s"[error] $path: unknown case id '$id' " +
            s"(known: ${TestCase.all.map(_.id).mkString(", ")})"))),
      )
    }
    (name, runs)
  }

  private final case class Args(pairs: Seq[Pair], suiteName: String, cases: Option[Seq[TestCase]])

  private def parseArgs(argv: Array[String]): Args = {
    var suite  = Option.empty[String]
    var filter = Option.empty[Seq[TestCase]]
    val positional = ArrayBuffer.empty[String]
    var i = 0
    while (i < argv.length) {
      argv(i) match {
        case "--suite" =>
          require(i + 1 < argv.length, "[error] --suite needs a suite JSON file")
          suite = Some(argv(i + 1)); i += 2
        // `--case grouped-acc,toroidal-acc` restricts the run to those rows of the
        // matrix. It is what lets one target per case exist without one config file per
        // case: the config files stay organised by array style, each holding both
        // fabrics, and the filter picks the fabric x acc combination out of them.
        case "--case" =>
          require(i + 1 < argv.length, "[error] --case needs one or more case ids")
          filter = Some(TestCase.parseFilter(argv(i + 1))); i += 2
        // An unknown flag is rejected rather than taken as a file name, so a stale
        // `--block` / `--no-block` invocation says what happened instead of trying to
        // load a config called "--no-block".
        case flag if flag.startsWith("--") =>
          sys.error(s"[error] unknown option $flag " +
            "(the block test moved to `runMain torus.AccSramDoubleBufferTest`)")
        case other => positional += other; i += 1
      }
    }
    suite match {
      case Some(s) =>
        require(positional.isEmpty,
          s"[error] --suite takes the pairs from the manifest; drop the positional ${positional.mkString(", ")}")
        val (name, pairs) = loadSuite(resolve(s, "test_suite"))
        Args(pairs, name, filter)
      case None =>
        val cfg  = positional.headOption.getOrElse("grouped_toroidal_scalar_systolic_array_acc.json")
        val gemm = positional.lift(1).getOrElse("gemm(16,16,16).json")
        // The third positional is the GEMV shape a mode-switch run's second phase uses.
        Args(Seq(Pair(cfg, gemm, positional.lift(2))), "single", filter)
    }
  }

  /**
   * The coverage matrix: one line per named case, whether or not this run drove it.
   *
   * A case reporting `0 pass 0 fail 0 skip` is called out explicitly. That line is the
   * whole reason the cases have names — it is the failure mode this summary exists to
   * make visible, and it is not an error, so it does not change the exit code.
   */
  private def printMatrix(results: Seq[CaseResult], filtered: Option[Seq[TestCase]]): Unit = {
    val shown = filtered.getOrElse(TestCase.all)
    println("\n══════════════════════════════════════════════════════")
    println(" Coverage matrix")
    println("══════════════════════════════════════════════════════")
    for (c <- shown) {
      val rows = results.filter(_.testCase.contains(c))
      val p    = rows.count(_.verdict.isPass)
      val f    = rows.count(_.verdict.isFail)
      val s    = rows.count(_.verdict.isSkip)
      val note = if (rows.isEmpty) "   ← NO COVERAGE in this run" else ""
      println(f"  ${c.id}%-22s $p%3d pass $f%3d fail $s%3d skip$note")
      println(f"  ${""}%-22s   proves: ${c.claim}")
    }
    val orphans = results.filter(_.testCase.isEmpty)
    if (orphans.nonEmpty) {
      println(f"  ${"(no case)"}%-22s ${orphans.size}%3d row(s) whose (fabric, schedule, acc) triple " +
        "has no runner at all")
      orphans.foreach(r => println(s"      ${r.label}"))
    }
  }

  def main(argv: Array[String]): Unit = {
    val args = parseArgs(argv)

    println("\n══════════════════════════════════════════════════════")
    println(s" Systolic array — end-to-end (suite: ${args.suiteName})")
    args.cases.foreach(cs => println(s" cases: ${cs.map(_.id).mkString(", ")}"))
    println("══════════════════════════════════════════════════════")

    val results = ArrayBuffer.empty[CaseResult]

    for (pair <- args.pairs) {
      val configPath = resolve(pair.config, "test_config")
      val gemmPath   = resolve(pair.gemm, "gemm")
      println(s"\n  config file : $configPath")
      println(s"  gemm file   : $gemmPath")

      val configs = ConfigLoader.loadFromResource(configPath)
      val gemms   = GemmLoader.loadFromResource(gemmPath)

      // A switch runs on a PAIR of shapes, and the switch — not the tiling — is what is
      // under test, so both files must hold exactly one entry. Making it a require on the
      // FILE rather than a `.head` in the runner keeps the decision in the data.
      val gemvs = pair.gemv.map { p =>
        val path = resolve(p, "gemm")
        println(s"  gemv file   : $path")
        val gs = GemmLoader.loadFromResource(path)
        require(gs.size == 1, s"[error] $path is a mode-switch GEMV shape, so it must hold exactly " +
          s"one entry, got ${gs.size}")
        require(gemms.size == 1, s"[error] $gemmPath is paired with a mode-switch GEMV shape, so it " +
          s"must hold exactly one entry, got ${gemms.size}")
        gs.head
      }

      var matched = 0

      for {
        g    <- configs
        gemm <- gemms
      } {
        val cfg = ConfigLoader.toSystolicArrayConfig(g)
        // A row that names a case asserts its SCHEDULE too: the shape files are ordinary
        // shapes with no "schedule" of their own, so this is what says a switch is being
        // run.
        val shaped = pair.kase match {
          case Some(k) => gemm.copy(schedule = Some(k.schedule))
          case None    => gemm
        }
        val spec  = GemmSpec(shaped, gemvs)
        val label = s"${g.name} × ${spec.name}"
        // The filter is applied on the case the (config, schedule, acc) triple resolves
        // to, so a config file holding several fabrics serves several cases and each
        // target picks its own rows out of it.
        val kase  = TestCase.of(cfg.fabric, shaped.scheduleOn(cfg), cfg.needAccumulator)
        // The row's own case selects which entries of a multi-fabric config file it
        // covers; --case then filters those rows further.
        val selected = pair.kase.forall(kase.contains)
        if (selected) matched += 1
        val wanted = selected && args.cases.forall(cs => kase.exists(cs.contains))
        if (wanted) {
          println(s"\n── $label ──")
          // A thrown exception becomes a FAIL row rather than aborting the suite, so one
          // bad config cannot silently cost coverage for everything after it.
          val res =
            try DataflowDriver.run(cfg, g.name, spec)
            catch {
              case e: Throwable =>
                println(s"  FAIL  $label: ${e.getMessage}")
                CaseResult(kase, label, Verdict.Fail(String.valueOf(e.getMessage)))
            }
          results += res
        }
      }

      // A row that names a case and matches nothing is a manifest error, not a quiet
      // no-op: it is the manifest being checked against the matrix.
      pair.kase.foreach { declared =>
        require(matched > 0,
          s"[error] ${pair.config} × ${pair.gemm} declares case '${declared.id}', but no config " +
            s"entry in it resolves to that case")
      }
    }

    println("\n══════════════════════════════════════════════════════")
    println(" Summary")
    println("══════════════════════════════════════════════════════")
    results.foreach { r =>
      val tag = r.verdict match {
        case Verdict.Pass    => "PASS"
        case Verdict.Fail(_) => "FAIL"
        case Verdict.Skip(_) => "SKIP"
      }
      println(f"  $tag  ${r.label}")
    }
    if (results.isEmpty) println("  (no rows in this suite)")

    printMatrix(results.toSeq, args.cases)

    // A Skip is neither a pass nor a failure: a run made entirely of skips exits zero
    // and says so loudly in the matrix above.
    if (results.exists(_.verdict.isFail)) sys.exit(1)
  }

}
