package torus.array

import spinal.core._
import spinal.lib.Delay

import torus.accumulator._
import torus.arithmetic._
import torus.buffer._
import torus.config._
import torus.core._
import torus.pe._
import torus.topology._

/**
 * Everything the two fabrics share: the top-level `io`, the module name, and the wiring
 * helpers whose shape does not depend on whether the array is Grouped or Toroidal.
 *
 * A concrete array is a Component mixing this in — [[GroupedSystolicArray]] or
 * [[ToroidalSystolicArray]] — and the split between them is exactly one thing: HOW
 * operands reach the PEs.
 * {{{
 *   Grouped    linear chains; the stagger is made at the EDGE by a SkewBuffer, so the
 *              helpers here (systolicWireA/B) are the whole story
 *   Toroidal   the far edge feeds back into the near one; the stagger comes from the
 *              wrap itself, so that array overrides wireA/wireB with its own modulo
 *              wiring and uses flat RegisterBuffers at the edge
 * }}}
 * Everything AFTER the operands — the partial-sum chain, output collection, the
 * accumulator — is common, which is why `connectPeChainReuse*`, `collectReuse*Outputs`
 * and [[attachAccumulatorIfNeeded]] live here and are `final`.
 *
 * Elaboration is a template method: the subclass builds its buffers and PEs, then calls
 * `wire(pes)`, which runs `wireA` / `wireB` / `wireControl` / `wireC` in that order. The
 * four are abstract because each fabric answers them differently; the helpers they are
 * built from are not.
 *
 * Dimensions are never computed here. Every port count comes from [[ArrayTopology]],
 * which is dispatched on (fabric x dataflow) and throws for any quantity its combination
 * has no meaning for — so a helper reaching for the wrong one fails at elaboration
 * rather than producing a plausible-looking array.
 */
trait SystolicArray[InputType <: Data, AccType <: Data] {

  def config: SystolicArrayConfig
  final def topology: ArrayTopology = config.topology

  implicit def arithmetic: Arithmetic[InputType, AccType]
  implicit def portType: PortTypeProvider[InputType, AccType]

  // Toroidal DualMode (low-m GEMV) only; every other array leaves this false, so its
  // io/PEs are byte-identical to before. When set, a per-VectorPE second-stream bus
  // and a shared select are exposed and muxed straight at each MAC. The bus sits on
  // whichever operand STREAMS: ReuseB streams A (`inputA2`/`selectA`), ReuseA mirrors
  // it and streams B (`inputB2`/`selectB`). The STATIONARY operand keeps its stock
  // port and gains `stationarySelect`, which picks broadcast vs normal on the
  // [[StationaryDemux]] the array puts in front of it.
  def dualMode: Boolean = false

  final protected def dualModeReuseA: Boolean = dualMode && config.isReuseA
  final protected def dualModeReuseB: Boolean = dualMode && config.isReuseB

  // The array-level dual-mode adder tree only exists when the contraction axis spans
  // MORE than one outer PE. With a single outer hop the intra-cluster adder tree
  // already reduces the whole axis, so the stock chain output IS the dual result —
  // no tap, no tree, no output mux; identical to the pre-reorg structure (which had
  // no skew registers there either, outer index 0 everywhere).
  final protected def dualModeTreeReuseA: Boolean = dualModeReuseA && config.outerPeCol > 1
  final protected def dualModeTreeReuseB: Boolean = dualModeReuseB && config.outerPeRow > 1

  // One `multPerPe`-wide dual-mode lane per VectorPE, ordered VectorPE-major:
  // ((r * outerPeCol + c) * ... ) * multPerPe, matching wireDualModeA/B below.
  final protected def dualModeInputDim: Int =
    config.totalVectorPeRow * config.totalVectorPeCol * config.multPerPe

  /**
   * The array's top-level ports. Most of them are CONDITIONAL (`generate`), because the
   * six (fabric x dataflow) combinations genuinely have different interfaces — a ReuseC
   * array has no `accControl` port at all, not a tied-off one. `generate` omits the port
   * entirely when the guard is false, so a driver reaching for the wrong control bus
   * fails to compile rather than driving something inert.
   *
   * The control port is the dataflow's own: `inputUpdateEnableA` captures the stationary
   * A operand (ReuseA), `inputUpdateEnableB` the stationary B (ReuseB), and
   * `inputAccEnableC` gates in-PE accumulation (ReuseC). Exactly one exists.
   */
  val io = new Bundle {

    val inputA = in Vec(portType.createInputTypeA, topology.systolicInputDimA)
    val inputB = in Vec(portType.createInputTypeB, topology.systolicInputDimB)

    val inputA2 = dualModeReuseB generate in { Vec(portType.createInputTypeA, dualModeInputDim) }
    val selectA = dualModeReuseB generate in { Bool() }

    val inputB2 = dualModeReuseA generate in { Vec(portType.createInputTypeB, dualModeInputDim) }
    val selectB = dualModeReuseA generate in { Bool() }

    // Dual-mode stationary-operand mode: True -> broadcast the first multiplier group
    // across the whole broadcast axis, False -> pass the full-width bus through. The
    // stationary DATA keeps the stock port (`inputB` for ReuseB, `inputA` for ReuseA);
    // only its meaning changes with this select.
    val stationarySelect = dualMode generate in { Bool() }

    val inputUpdateEnableA = config.isReuseA generate in {
      Vec(Bool(), topology.numUpdateEnableA)
    }
    val inputUpdateEnableB = config.isReuseB generate in {
      Vec(Bool(), topology.numUpdateEnableB)
    }
    val inputAccEnableC = config.isReuseC generate in {
      Vec(Bool(), topology.numAccEnableC)
    }


    val accControl = config.needAccumulator generate in {
      Vec(AccControl(), topology.numAccControl)
    }

    // One port either way, but its WIDTH says whether an accumulator is attached: with
    // one, `outputC` carries a sum over all K passes and is `log2Up(contractionPasses)`
    // bits wider than a single fabric pass. Drivers must size their readback from the
    // same config, not from the systolic width.
    val outputC = out Vec(
      if (config.needAccumulator) portType.createAccumulatorOutputTypeC
      else portType.createSystolicOutputTypeC,
      topology.systolicOutputDimC
    )

  }

  // `io` is declared in this TRAIT, so the SpinalHDL naming plugin never sees a `val io`
  // in the array class and leaves the bundle anonymous — the generated top's ports then
  // fall back to auto names taken from whatever they drive (`_zz_io_input_0`, ...).
  // Name it explicitly so every array top keeps its `io_inputA_0` style port names.
  io.setName("io")

  final protected def buildDefinitionName(): String = config match {
    case grouped: GroupedConfig =>
      s"${config.numericType.name}_${config.dataflow}_Grouped_SystolicArray_" +
        s"${grouped.outerPeRow}x${grouped.outerPeCol}x" +
        s"${grouped.localVectorPeRow}x${grouped.localVectorPeCol}x" +
        s"${grouped.multPerPe}"
    case toroidal: ToroidalConfig =>
      s"${config.numericType.name}_${config.dataflow}_Toroidal_SystolicArray_" +
        s"${toroidal.outerPeRow}x${toroidal.outerPeCol}x" +
        s"${toroidal.localVectorPeRow}x${toroidal.localVectorPeCol}x" +
        s"${toroidal.multPerPe}"
  }

  final protected def createVectorPe(): Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]] = {
    Vector.tabulate(config.outerPeRow, config.outerPeCol) { (r, c) =>
      new VectorPE(
        outerIndex = OuterPeIndex(r, c),
        innerIndex = InnerIndex(0, 0),
        vectorPortMask = topology.standAloneVectorPePortMask(r, c),
        config = config,
        dualMode = dualMode,
      )
    }
  }

  /**
   * Elaboration entry point, called by the subclass once its buffers and PEs exist.
   * Order matters only in that `wireC` reads PE outputs the earlier steps created.
   */
  protected def wire(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    wireA(pes)
    wireB(pes)
    wireControl(pes)
    wireC(pes)
  }

  protected def wireA(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit
  protected def wireB(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit
  protected def wireControl(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit
  protected def wireC(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit
  protected def connectPeChain(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit
  protected def connectPostProcessorToOutput(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit

/**
   * Linear west-to-east A wiring: column 0 is fed from the edge, every other column
   * takes the previous column's `outputA`. A therefore walks ACROSS the array, one
   * registered hop per cluster, and only column 0 costs top-level ports.
   *
   * `inputASource` is a function rather than a `Vec` so the caller decides what the edge
   * is — normally `preprocessor.io.output(_)`, whichever buffer the dataflow put there.
   *
   * Flat-index convention, used identically by every A-side buffer and by
   * `topology.skewBufferPortsPerStepA`: cluster row `r`, sub-row `a`, lane `p` lives at
   * `r * localVectorPeRow * multPerPe + a * multPerPe + p`. Inside a PE the same element
   * is just `a * multPerPe + p`, since a PE only knows its own sub-rows — which is why
   * the two loops below index with and without the `multipliersPerOuterPe` offset.
   */
  final protected def systolicWireA(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
    inputASource: Int => InputType,
  ): Unit = {

    // West edge: every cluster row draws its own window of the bus.
    for {
      r <- 0 until config.outerPeRow
      a <- 0 until config.localVectorPeRow
      p <- 0 until config.multPerPe
    } {
      val multiplierIndex = a * config.multPerPe + p
      val multipliersPerOuterPe = r * config.localVectorPeRow * config.multPerPe
      pes(r)(0).io.inputA(multiplierIndex) := inputASource(multiplierIndex + multipliersPerOuterPe)
    }

    // Interior: pure neighbour-to-neighbour forwarding, no edge ports involved.
    for {
      r <- 0 until config.outerPeRow
      c <- 1 until config.outerPeCol
      a <- 0 until config.localVectorPeRow
      p <- 0 until config.multPerPe
    } {
      val multiplierIndex = a * config.multPerPe + p
      pes(r)(c).io.inputA(multiplierIndex) := pes(r)(c - 1).io.outputA(multiplierIndex)
    }
  }

  /**
   * North-to-south mirror of [[systolicWireA]]: row 0 from the edge, every other row
   * from the row above. Same flat-index convention on the column axis — cluster column
   * `c`, sub-column `b`, lane `p` at `c * localVectorPeCol * multPerPe + b * multPerPe + p`.
   */
  final protected def systolicWireB(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
    inputBSource: Int => InputType,
  ): Unit = {

    val outerPeRow      = config.outerPeRow
    val outerPeCol      = config.outerPeCol
    val localVectorPeCol = config.localVectorPeCol
    val multPerPe        = config.multPerPe

    for {
      c <- 0 until outerPeCol
      b <- 0 until localVectorPeCol
      p <- 0 until multPerPe
    } {
      val multiplierIndex       = b * multPerPe + p
      val multipliersPerOuterPe = c * localVectorPeCol * multPerPe
      pes(0)(c).io.inputB(multiplierIndex) := inputBSource(multiplierIndex + multipliersPerOuterPe)
    }

    for {
      r <- 1 until outerPeRow
      c <- 0 until outerPeCol
      b <- 0 until localVectorPeCol
      p <- 0 until multPerPe
    } {
      val multiplierIndex = b * multPerPe + p
      pes(r)(c).io.inputB(multiplierIndex) := pes(r - 1)(c).io.outputB(multiplierIndex)
    }
  }

  final protected def systolicWireUpdateEnableA(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
    enableSource: Vec[Bool],
  ): Unit = {

    // Same single cycle the inline `RegNext` used to give, but inside a named buffer
    // module so synthesis sees one uniform register stage on every array input rather
    // than loose auto-named flops in the array itself.
    val buffer = RegisterBuffer.forControl("UpdateEnableA_SyncBuffer", topology.numUpdateEnableA)
    buffer.io.input := enableSource
    val registeredSource = buffer.io.output

    for {
      r <- 0 until config.outerPeRow
      a <- 0 until config.localVectorPeRow
    } pes(r)(0).io.inputUpdateEnableA(a) := registeredSource(r * config.localVectorPeRow + a)

    for {
      r <- 0 until config.outerPeRow
      c <- 1 until config.outerPeCol
      a <- 0 until config.localVectorPeRow
    } pes(r)(c).io.inputUpdateEnableA(a) := pes(r)(c - 1).io.outputUpdateEnableA(a)
  }

  final protected def systolicWireUpdateEnableB(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
    enableSource: Vec[Bool],
  ): Unit = {

    // Mirror of systolicWireUpdateEnableA — one buffered cycle, same as the inline flops.
    val buffer = RegisterBuffer.forControl("UpdateEnableB_SyncBuffer", topology.numUpdateEnableB)
    buffer.io.input := enableSource
    val registeredSource = buffer.io.output

    for {
      c <- 0 until config.outerPeCol
      b <- 0 until config.localVectorPeCol
    } pes(0)(c).io.inputUpdateEnableB(b) := registeredSource(c * config.localVectorPeCol + b)

    for {
      r <- 1 until config.outerPeRow
      c <- 0 until config.outerPeCol
      b <- 0 until config.localVectorPeCol
    } pes(r)(c).io.inputUpdateEnableB(b) := pes(r - 1)(c).io.outputUpdateEnableB(b)
  }

  //TODO: merge skewing logic with skew buffer after validation signal is added
  final protected def systolicWireAccEnableC(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
    enableSource: Vec[Bool],
    applySkew: Boolean = false,
  ): Unit = {
    // Buffered entry stage, same delay per port as the inline `Delay`s it replaces.
    // Which buffer depends on the skew: Grouped needs column `c` delayed by `c + 1` to
    // follow the operand skew, which is exactly SkewBuffer's isMinDepthFirst law
    // (Delay(input(portsPerDelay*i + n), i + 1)); Toroidal needs a flat one cycle.
    val buffer: Component with SkewBufferIO[Bool] =
      if (applySkew)
        SkewBuffer.forControl(
          bufferName    = "AccEnableC_SkewBuffer",
          delayDepth    = config.outerPeCol,
          portsPerDelay = config.localVectorPeCol,
        )
      else
        RegisterBuffer.forControl("AccEnableC_SyncBuffer", topology.numAccEnableC)

    buffer.io.input := enableSource

    for {
      c <- 0 until config.outerPeCol
      b <- 0 until config.localVectorPeCol
    } pes(0)(c).io.inputAccEnableC(b) := buffer.io.output(c * config.localVectorPeCol + b)

    for {
      r <- 1 until config.outerPeRow
      c <- 0 until config.outerPeCol
      b <- 0 until config.localVectorPeCol
    } pes(r)(c).io.inputAccEnableC(b) := pes(r - 1)(c).io.outputAccEnableC(b)
  }

  /**
   * ReuseA partial-sum chain: C flows WEST to EAST, the same direction A travels, so a
   * cluster adds its products to the sum arriving from its left neighbour. Column 0 has
   * no `inputC` (its `PortEnableMask.withInputPortC` is false), which is why the loop
   * starts at 1. The chain is per sub-row `a`: the lvR sub-rows are independent
   * reductions running side by side.
   */
  final protected def connectPeChainReuseA(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
  ): Unit = {

    for {
      r <- 0 until config.outerPeRow
      c <- 1 until config.outerPeCol
      a <- 0 until config.localVectorPeRow
    } pes(r)(c).io.inputC(a) := pes(r)(c - 1).io.outputC(a)
  }

  /** ReuseB mirror of [[connectPeChainReuseA]]: C flows NORTH to SOUTH, per sub-column `b`. */
  final protected def connectPeChainReuseB(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
  ): Unit = {

    for {
      r <- 1 until config.outerPeRow
      c <- 0 until config.outerPeCol
      b <- 0 until config.localVectorPeCol
    } pes(r)(c).io.inputC(b) := pes(r - 1)(c).io.outputC(b)
  }

  /**
   * Gather the finished ReuseA sums off the EAST edge — the last column, where the
   * west-to-east chain ends. Output index `r * localVectorPeRow + a` is the physical row,
   * matching what the C de-skew buffer and the accumulator index by.
   *
   * `target` is passed in rather than written to `io.outputC` directly because what
   * comes next differs: Grouped hands it to the de-skew buffer, Toroidal may hand it to
   * the dual-mode output mux, and only then does either reach the port.
   */
  final protected def collectReuseAOutputs(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
    target: Vec[AccType],
  ): Unit = {
    config.requireReuseA()

    for {
      r <- 0 until config.outerPeRow
      a <- 0 until config.localVectorPeRow
    } {
      val outputIndex = r * config.localVectorPeRow + a
      val outputPe    = pes(r)(config.outerPeCol - 1)
      target(outputIndex) := outputPe.io.outputC(a)
    }
  }

  /** ReuseB mirror: the SOUTH edge (last row), indexed by physical column `c * lvC + b`. */
  final protected def collectReuseBOutputs(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
    target: Vec[AccType],
  ): Unit = {

    for {
      c <- 0 until config.outerPeCol
      b <- 0 until config.localVectorPeCol
    } {
      val outputIndex = c * config.localVectorPeCol + b
      val outputPe    = pes(config.outerPeRow - 1)(c)
      target(outputIndex) := outputPe.io.outputC(b)
    }

  }

  /**
   * ReuseC drain off the south edge. Unlike ReuseA/ReuseB this is NOT the end of a
   * spatial reduction — each PE already holds a finished output element — so a bottom
   * cluster presents all `numVectorPePerGroupPe` of its VectorPEs, and the array's
   * results arrive over successive cycles as the chain shifts them down.
   */
  final protected def collectReuseCOutputs(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
    target: Vec[AccType],
  ): Unit = {
    for {
      c <- 0 until config.outerPeCol
      idx <- 0 until topology.numVectorPePerGroupPe
    } {
      val outputIndex = c * topology.numVectorPePerGroupPe + idx
      val outputPe    = pes(config.outerPeRow - 1)(c)
      target(outputIndex) := outputPe.io.outputC(idx)
    }
  }

  /**
   * "Two big SRAMs" accumulator: a single [[AccSramDoubleBuffer]] (two wide
   * synchronous SRAM banks) accumulates ALL output ports together — two memory
   * macros total, rather than a pair per output port. When `needAccumulator` is
   * false (e.g. Grouped / Toroidal ReuseC) this is a pure pass-through.
   *
   * The register-based (shift-register [[AccTap]] + double-buffer) accumulator
   * has been removed; the SRAM ping-pong is now the standard and only
   * accumulator, so this lives directly on [[SystolicArray]].
   *
   * The two banks share one address/word, so all ports must be aligned (same
   * accumulator slot per cycle), and the shared control is taken from port 0's
   * accControl. Both architectures satisfy this for ReuseA and ReuseB, by
   * different means:
   *
   *  - Grouped has a TypeC de-skew buffer followed by an [[AccTap]] that aligns every
   *    output port for both dataflows; the AccTap derives the shared control.
   *  - Toroidal has no de-skew buffer, but its accControl enters one array edge for
   *    every port simultaneously and propagates a uniform number of hops to the
   *    output edge (down columns for ReuseB, across rows for ReuseA), so every
   *    port's control carries identical timing and port 0's stands in for all.
   *
   * In all cases ReuseC needs no accumulator.
   *
   * The Grouped path owns the [[AccTap]] internally: Grouped passes the de-skewed
   * `collected` ports and a single-element `collectedAccControl` (port 0's
   * accControl), and the AccTap derives the de-skew-matched shared control here.
   * Its data path is pass-through (`output == input`) and every
   * `delayedAccControl(i)` carries the same total delay, so `delayedAccControl(0)`
   * is the correct shared control for the single-address big SRAM. Toroidal ports are
   * already aligned and feed the buffer directly with port 0's
   * `collectedAccControl`.
   */
  final protected def attachAccumulatorIfNeeded(
    collected: Vec[AccType],
    collectedAccControl: Vec[AccControl],
    outputC: Vec[AccType],
  ): Unit = {
    if (config.needAccumulator) {
      val numPorts = topology.systolicOutputDimC
      val bigBuffer = new AccSramDoubleBuffer(config, numPorts)

      // all ports walk the same accumulator slot in lockstep, so one shared
      // control drives both banks. The aligned-dataflow guarantee differs per
      // architecture (see the method doc).
      val sharedControl = config match {
        case _: GroupedConfig =>
          // Grouped's de-skew buffer + AccTap aligns ports for ReuseA and ReuseB alike.
          require(
            config.isReuseAB,
            "[error] Grouped AccSramDoubleBuffer supports ReuseA / ReuseB only " +
              "(ReuseC needs no accumulator)."
          )
          val accTap = new AccTap[InputType, AccType](config)
          accTap.io.input        := collected
          accTap.io.accControlIn := collectedAccControl(0)
          bigBuffer.io.input := accTap.io.output
          accTap.io.delayedAccControl(0)

        case _: ToroidalConfig =>
          // Toroidal has no de-skew buffer, but ReuseA / ReuseB both deliver every
          // port's control with identical timing (uniform hop count to the
          // output edge), so port 0's control drives all banks.
          require(
            config.isReuseAB,
            "[error] Toroidal AccSramDoubleBuffer supports ReuseA / ReuseB only " +
              "(ReuseC needs no accumulator)."
          )
          bigBuffer.io.input := collected
          collectedAccControl(0)
      }

      bigBuffer.io.accControl := sharedControl

      outputC := bigBuffer.io.output
    } else {
      outputC := collected
    }
  }

}
