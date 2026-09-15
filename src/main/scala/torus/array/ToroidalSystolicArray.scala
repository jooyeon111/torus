package torus.array

import spinal.core._
import spinal.lib.Delay

import torus.accumulator._
import torus.arithmetic._
import torus.buffer._
import torus.config._
import torus.core._
import torus.pe._

/**
 * Toroidal systolic array. `dualMode` expands it into the DualMode variant — an
 * otherwise STOCK Toroidal ReuseA / ReuseB fabric with two additions, both outside the
 * PEs (their single MAC unit stays exactly stock):
 *
 *  1. Stationary-operand broadcast (low-m GEMV): the small stationary tile (physically
 *     held in a weight SRAM) is one vertical multiplier group (`multPerPe` wide).
 *     [[StationaryDemux]], in front of the stationary operand's own port, copies it
 *     across every group of the broadcast axis. The port stays full-width and
 *     `stationarySelect` picks the mode: broadcast copies the first group across the
 *     whole axis (only group 0 used); normal passes the full bus through so each group
 *     gets its own stationary values.
 *
 *  2. Dual-stream input, single MAC: instead of duplicating the datapath, the one MAC
 *     is kept fully utilised by TIME-SHARING it between two input streams. A 1-bit
 *     select muxes the stock stream against the per-VectorPE second stream at each
 *     MAC. Stream-0 partial sums ride the stock C-chain; stream-1 lanes arrive fully
 *     aligned and reduce through the array-level [[DualModeAdderTree]] instead, with
 *     the select (delayed by the dual path's depth) muxing the two onto the shared
 *     `outputC` — a time-demux at the port. No second output / drain / propagation is
 *     needed.
 *
 * ReuseA and ReuseB are MIRROR REFLECTIONS of each other and dual mode mirrors with
 * them; ReuseC rejects dual mode. Everything dual-mode below is written in terms of the
 * STATIONARY operand (held in the PEs) and the STREAMED one (walking the fabric):
 *
 *              stationary   streamed   second-stream port   stationary port   broadcast axis
 *   ReuseB         B           A        inputA2 / selectA        inputB           columns
 *   ReuseA         A           B        inputB2 / selectB        inputA           rows
 *
 * The dual-mode config carries the same dimensions as the plain one — build the array
 * as `new ToroidalSystolicArray(cfg.toToroidalConfig, dualMode = true)`.
 */
class ToroidalSystolicArray[InputType <: Data, AccType <: Data](
  override val config: SystolicArrayConfig,
  override val dualMode: Boolean = false,
)(
  implicit val arithmetic: Arithmetic[InputType, AccType],
  implicit val portType: PortTypeProvider[InputType, AccType],
) extends Component with SystolicArray[InputType, AccType] {

  require(!dualMode || config.isReuseAB,
    s"[error] Toroidal DualMode supports ReuseA / ReuseB only, got ${config.dataflow}")

  setDefinitionName(buildDefinitionName() + (if (dualMode) "_DualMode" else ""))

  private val pes = createOuterPes()

  // ── Dual-mode entry stages (dualMode only; None leaves the array byte-identical) ──
  //
  // The stationary operand's broadcast demux. Purely combinational and ahead of the
  // stationary preprocessor, so the weight load keeps its stock timing — a second
  // register stage here would desynchronise it from `inputUpdateEnable*`.
  private val stationaryDemux: Option[StationaryDemux[InputType]] =
    if (dualMode) Some(StationaryDemux.forStationary[InputType, AccType](config)) else None

  // Registered entry stage for the second stream. Without it the lanes would run from
  // the top-level port straight into every MAC (they are consumed fully aligned, port
  // -> mux -> multiplier, with no per-PE registers). It also makes the dual path
  // exactly as deep as the stock operand path, which crosses its own input buffer —
  // which is what lets `accControl` keep the stock timing instead of having to lead
  // its data by a cycle.
  private val dualStreamBuffer: Option[RegisterBuffer[InputType]] =
    if (dualMode) Some(RegisterBuffer.forDualStream[InputType, AccType](
      tileType = if (config.isReuseB) TileType.TypeA else TileType.TypeB,
      numPort  = dualModeInputDim,
    )) else None

  // One buffer for both 1-bit mode selects, so `select` does not fan out unregistered
  // to every MAC mux. The stream select takes the SAME single cycle as its stream, so
  // their relative alignment is untouched. `stationarySelect` picks the demux mode and
  // is held for a whole run, so its extra cycle only means it must be set one cycle
  // before the first weight-load enable.
  private val StreamSelectIdx     = 0
  private val StationarySelectIdx = 1
  private val selectBuffer: Option[RegisterBuffer[Bool]] =
    if (dualMode) Some(RegisterBuffer.forControl("DualModeSelect_SyncBuffer", 2)) else None

  //Toroidal Systolic Array does not need skew buffer
  //Toroidal ReuseC needs wrapping buffer for forwarding Tile A and Tile B
  private val wrapBufferA: Option[WrapBuffer[InputType, AccType]] =
    if (config.isReuseC) Some(new WrapBuffer[InputType, AccType](TileType.TypeA, config)) else None
  private val wrapBufferB: Option[WrapBuffer[InputType, AccType]] =
    if (config.isReuseC) Some(new WrapBuffer[InputType, AccType](TileType.TypeB, config)) else None

  //Toroidal ResueA and ReuseB is just for register buffer
  private val preprocessorA: Component with SkewBufferIO[InputType] = createPreprocessorA()
  private val preprocessorB: Component with SkewBufferIO[InputType] = createPreprocessorB()

  //The accumulator control bus enters through the same kind of buffer module as the
  //operand buses, so synthesis sees one uniform register stage on every array input
  //(see systolicWireAccControl). Same one cycle the inline RegNext used to give.
  private val preprocessorAccControl: Option[RegisterBuffer[AccControl]] =
    if (config.needAccumulator) Some(RegisterBuffer.forAccControl(config)) else None

  wireDualModeEntry()
  wire(pes)

  /**
   * Feed the dual-mode entry stages from the top-level ports. Their outputs are what
   * the fabric wiring below sees: [[wireDualModeA]] / [[wireDualModeB]] take the
   * buffered stream and select, [[stationaryOperand]] the demuxed stationary bus, and
   * [[muxDualModeOutputs]] the buffered select — so every dual-mode path crosses
   * exactly the one register stage the stock operand path does.
   */
  private def wireDualModeEntry(): Unit = if (dualMode) {
    val selects = selectBuffer.get
    selects.io.input(StreamSelectIdx)     := (if (config.isReuseB) io.selectA else io.selectB)
    selects.io.input(StationarySelectIdx) := io.stationarySelect

    dualStreamBuffer.get.io.input := (if (config.isReuseB) io.inputA2 else io.inputB2)
  }

  /**
   * The stationary operand as the fabric sees it: in dual mode through the broadcast
   * demux, otherwise the port itself. `port` is `io.inputB` for ReuseB and `io.inputA`
   * for ReuseA — the demux is full-width on both sides, so the width never changes.
   */
  private def stationaryOperand(port: Vec[InputType]): Vec[InputType] = stationaryDemux match {
    case Some(demux) =>
      demux.io.input  := port
      demux.io.select := selectBuffer.get.io.output(StationarySelectIdx)
      demux.io.output
    case None => port
  }

  // A 1x1 cluster would be a ClusterPE wrapping a single VectorPE, so the wrapper is
  // skipped and the VectorPE is placed in the mesh directly — one less level of netlist
  // hierarchy, and the same `ProcessingElementIO` either way, so nothing below notices.
  private def createOuterPes(): Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]] = {
    if (config.isStandAloneVectorPeConfig) {
      createVectorPe()
    } else {
      Vector.tabulate(config.outerPeRow, config.outerPeCol) { (r, c) =>
        new ClusterPE(
          outerPeIndex = OuterPeIndex(r, c),
          portMask = topology.outerPePortMask(OuterPeIndex(r, c)),
          config = config,
          dualMode = dualMode,
        )
      }
    }
  }

  /**
   * No [[SkewBuffer]] appears anywhere in this class, which is the structural difference
   * from [[GroupedSystolicArray]]: the operand stagger a Grouped array manufactures at
   * its edge is here produced by the wrap wiring inside the fabric (see [[wireA]]), so a
   * flat one-cycle [[RegisterBuffer]] is all ReuseA / ReuseB need at the boundary.
   *
   * ReuseC instead puts its [[WrapBuffer]] in this slot — it is a `SkewBufferIO` too, so
   * it substitutes cleanly, and its cells' registers give the same one cycle while also
   * closing each line of the torus into a ring.
   */
  private def createPreprocessorA(): Component with SkewBufferIO[InputType] = config.dataflow match {
    case Dataflow.ReuseA | Dataflow.ReuseB =>
      RegisterBuffer.forInput[InputType, AccType](TileType.TypeA, config)
    case Dataflow.ReuseC =>
      wrapBufferA.get
  }

  /** Mirror of [[createPreprocessorA]] on the B side. */
  private def createPreprocessorB(): Component with SkewBufferIO[InputType] = config.dataflow match {
    case Dataflow.ReuseA | Dataflow.ReuseB =>
      RegisterBuffer.forInput[InputType, AccType](TileType.TypeB, config)
    case Dataflow.ReuseC =>
      wrapBufferB.get
  }

/**
   * A-operand wiring, and the place the "toroidal" in the name actually lives.
   *
   * The three dataflows use this slot completely differently:
   *
   *  - ReuseA — A is STATIONARY, captured into the PEs rather than circulated, so it
   *    takes the plain linear [[SystolicArray.systolicWireA]] chain. No wrap.
   *  - ReuseB — A STREAMS, and the wrap below is what replaces a Grouped array's skew
   *    buffer.
   *  - ReuseC — both operands circulate; the linear chain is reused, but its far end is
   *    fed back into [[WrapBuffer]] so each row closes into a ring.
   *
   * The ReuseB wrap, which is the part worth reading slowly: a cluster's A input is
   * `localVectorPeRow` groups of `multPerPe` lanes, and instead of every group entering
   * from the edge, only GROUP 0 does. Group `g > 0` of cluster `c` is group `g - 1` of
   * cluster `c + 1` — the neighbour to the EAST, modulo `outerPeCol`, so column
   * `outerPeCol - 1` reaches back around to column 0. The result is one helical chain
   * threading the whole mesh: within a row the operands step west through the groups,
   * and where a row's chain runs out it is picked up DIAGONALLY, from the last group of
   * the cluster up and to the right (`pes(r-1)(neighborCol)`).
   * {{{
   *   row r-1   ... [g0 g1 g2] ──┐   (its LAST group)
   *                              ↓
   *   row r     port ─> [g0 g1 g2] <─ [g0 g1 g2] <─ ...   (east neighbours, wrapping)
   * }}}
   * Two consequences, both the reason the wrap exists:
   *  - Each cluster sees its operand one hop later than the cluster feeding it, so the
   *    diagonal wavefront a Grouped array buys with a SkewBuffer is free here.
   *  - Edge ports cost `outerPeCol * multPerPe` instead of the full array width — which
   *    is exactly what `ToroidalReuseBTopology.systolicInputDimA` declares, and why the
   *    lvC broadcast columns add no boundary ports.
   *
   * The top cluster row is the only one fed from outside; every other row is fed by the
   * row above it, which is also why `outerPeOutputDimA` drops one group on the LAST
   * cluster row — there is no row below to consume it.
   *
   * [[wireB]] is the exact mirror (rows for columns, ReuseA for ReuseB).
   */
  override def wireA(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    // ReuseA holds A stationary, so in dual mode the A port is the one the broadcast
    // demux sits in front of; every other case passes the port straight through.
    preprocessorA.io.input := (if (dualModeReuseA) stationaryOperand(io.inputA) else io.inputA)

    config.dataflow match {
      case Dataflow.ReuseA =>
        systolicWireA(pes, preprocessorA.io.output(_))

      case Dataflow.ReuseB =>
        // Cluster row 0 — the only row fed from the port at all.
        for {
          c <- 0 until config.outerPeCol
          i <- 0 until topology.outerPeInputDimA
        } {
          if (i < config.multPerPe) {
            // Group 0: fresh from the edge. Only `outerPeCol * multPerPe` ports total.
            pes(0)(c).io.inputA(i) := preprocessorA.io.output(c * config.multPerPe + i)
          } else {
            // Groups 1..: one group EARLIER in the east neighbour, wrapping at the edge.
            // `i - multPerPe` is that shift; the modulo is the torus closing the row.
            val neighborCol = (c + 1) % config.outerPeCol
            pes(0)(c).io.inputA(i) := pes(0)(neighborCol).io.outputA(i - config.multPerPe)
          }
        }

        // Every other cluster row: no edge ports, fed entirely from the row above.
        for {
          r <- 1 until config.outerPeRow
          c <- 0 until config.outerPeCol
          i <- 0 until topology.outerPeInputDimA
        } {
          val neighborCol = (c + 1) % config.outerPeCol
          if (i < config.multPerPe) {
            // Group 0 comes DIAGONALLY, from the up-and-right cluster's LAST group
            // (`outerPeInputDimA - multPerPe` is that group's base). This is the splice
            // that joins each row's helix to the next — the row above has walked its
            // operands all the way through its groups, and this picks them up at the end.
            pes(r)(c).io.inputA(i) := pes(r-1)(neighborCol).io.outputA(i + topology.outerPeInputDimA - config.multPerPe)
          } else {
            // Groups 1.. behave exactly as in row 0: same row, east neighbour, shift one.
            pes(r)(c).io.inputA(i) := pes(r)(neighborCol).io.outputA(i - config.multPerPe)
          }
        }

      case Dataflow.ReuseC =>
        val wrapA = wrapBufferA.get
        // Close the ring: the chain built by `systolicWireA` below is linear, and this is
        // the wire that joins its east end back to its west end through the wrap buffer.
        // The buffer's per-line mux then chooses stream vs recirculate each cycle.
        // Rotate the rightmost cluster's A row-group (lvR sub-rows × mult) back to the
        // wrap buffer, one wrap cell per PHYSICAL row (r*lvR + a).
        for {
          r <- 0 until config.outerPeRow
          a <- 0 until config.localVectorPeRow
          p <- 0 until config.multPerPe
        } {
          wrapA.io.rotatedInput((r * config.localVectorPeRow + a) * config.multPerPe + p) :=
            pes(r)(config.outerPeCol - 1).io.outputA(a * config.multPerPe + p)
        }
        systolicWireA(pes, preprocessorA.io.output(_))
    }

    if (dualModeReuseB) wireDualModeA(pes)
  }

  /**
   * Toroidal DualMode dual-stream injection, ReuseB side. The stationary operand is
   * duplicated across all columns ([[stationaryOperand]]'s demux), so each VectorPE must
   * be fed its own input tile. This slices the buffered per-VectorPE second stream per
   * outer PE and broadcasts the buffered stream select; each outer PE (standalone
   * VectorPE or ClusterPE) fans its slice to its inner VectorPE(s), and the actual
   * inputA/inputA2 mux lives inside VectorPE, at the MAC. Ordering is
   * VectorPE-major, matching the top-level `inputA2` port.
   *
   * [[wireDualModeB]] is the exact mirror for ReuseA.
   */
  private def wireDualModeA(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
  ): Unit = {
    val perOuterPe = config.localVectorPeRow * config.localVectorPeCol * config.multPerPe
    val stream     = dualStreamBuffer.get.io.output
    val select     = selectBuffer.get.io.output(StreamSelectIdx)

    for {
      r <- 0 until config.outerPeRow
      c <- 0 until config.outerPeCol
    } {
      val base = (r * config.outerPeCol + c) * perOuterPe
      for (i <- 0 until perOuterPe)
        pes(r)(c).io.inputA2(i) := stream(base + i)
      pes(r)(c).io.selectA := select
    }
  }

  /** ReuseA mirror of [[wireDualModeA]]: same VectorPE-major slicing, on the B lanes. */
  private def wireDualModeB(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
  ): Unit = {
    val perOuterPe = config.localVectorPeRow * config.localVectorPeCol * config.multPerPe
    val stream     = dualStreamBuffer.get.io.output
    val select     = selectBuffer.get.io.output(StreamSelectIdx)

    for {
      r <- 0 until config.outerPeRow
      c <- 0 until config.outerPeCol
    } {
      val base = (r * config.outerPeCol + c) * perOuterPe
      for (i <- 0 until perOuterPe)
        pes(r)(c).io.inputB2(i) := stream(base + i)
      pes(r)(c).io.selectB := select
    }
  }

  /**
   * Exact mirror of [[wireA]] with the axes swapped: B streams under ReuseA, and its
   * helix runs SOUTH (neighbour row `(r + 1) % outerPeRow`) with the diagonal splice
   * coming from the cluster down-and-left, `pes(neighborRow)(c - 1)`. Cluster COLUMN 0
   * is the only one fed from the port, so the edge costs `outerPeRow * multPerPe` —
   * `ToroidalReuseATopology.systolicInputDimB`. Read [[wireA]]'s doc for the reasoning;
   * nothing here differs but the axis.
   */
  override def wireB(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    // Mirror of wireA: ReuseB holds B stationary, so the demux sits on the B port.
    preprocessorB.io.input := (if (dualModeReuseB) stationaryOperand(io.inputB) else io.inputB)

    config.dataflow match {
      case Dataflow.ReuseA =>
        // Cluster column 0 — the only column fed from the port.
        for {
          r <- 0 until config.outerPeRow
          i <- 0 until topology.outerPeInputDimB
        } {
          if (i < config.multPerPe) {
            pes(r)(0).io.inputB(i) := preprocessorB.io.output(r * config.multPerPe + i)
          } else {
            // Groups 1..: one group earlier in the SOUTH neighbour, wrapping at the edge.
            val neighborRow = (r + 1) % config.outerPeRow
            pes(r)(0).io.inputB(i) := pes(neighborRow)(0).io.outputB(i - config.multPerPe)
          }
        }

        // Every other cluster column: fed entirely from the column to its left.
        for {
          r <- 0 until config.outerPeRow
          c <- 1 until config.outerPeCol
          i <- 0 until topology.outerPeInputDimB
        } {
          val neighborRow = (r + 1) % config.outerPeRow
          if (i < config.multPerPe) {
            // Diagonal splice: the down-and-left cluster's LAST group, joining this
            // column's helix to the previous one.
            pes(r)(c).io.inputB(i) := pes(neighborRow)(c-1).io.outputB(i + topology.outerPeInputDimB - config.multPerPe)

          } else {
            pes(r)(c).io.inputB(i) := pes(neighborRow)(c).io.outputB(i - config.multPerPe)
          }
        }

      case Dataflow.ReuseB =>
        systolicWireB(pes, preprocessorB.io.output(_))

      case Dataflow.ReuseC =>
        val wrapB = wrapBufferB.get
        // Closes the B ring, exactly as the A case above closes the A ring.
        // Rotate the bottom cluster's B column-group (lvC sub-cols × mult) back to the
        // wrap buffer, one wrap cell per PHYSICAL column (c*lvC + b).
        for {
          c <- 0 until config.outerPeCol
          b <- 0 until config.localVectorPeCol
          p <- 0 until config.multPerPe
        } {
          wrapB.io.rotatedInput((c * config.localVectorPeCol + b) * config.multPerPe + p) :=
            pes(config.outerPeRow - 1)(c).io.outputB(b * config.multPerPe + p)
        }
        systolicWireB(pes, preprocessorB.io.output(_))
    }

    if (dualModeReuseA) wireDualModeB(pes)
  }

  override def wireControl(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        systolicWireUpdateEnableA(pes, io.inputUpdateEnableA)
        if (config.needAccumulator) systolicWireAccControl(pes, io.accControl)

      case Dataflow.ReuseB =>
        systolicWireUpdateEnableB(pes, io.inputUpdateEnableB)
        if (config.needAccumulator) systolicWireAccControl(pes, io.accControl)

      case Dataflow.ReuseC =>
        systolicWireAccEnableC(pes, io.inputAccEnableC)

        val wrapA = wrapBufferA.get
        val wrapB = wrapBufferB.get

        // selInput is the streaming-vs-rotate gate; one per wrap cell = one per
        // physical row (A) / column (B). accEnableC is indexed per output column
        // (numAccEnableC == totalVectorPeCol); the modulo keeps the row-indexed wrapA
        // in range and reduces to the direct index for square (lvR==lvC) arrays.
        for (g <- 0 until topology.wrapBufferNumCell(TileType.TypeA))
          wrapA.io.selInput(g) := io.inputAccEnableC(g % topology.numAccEnableC)

        for (g <- 0 until topology.wrapBufferNumCell(TileType.TypeB))
          wrapB.io.selInput(g) := io.inputAccEnableC(g % topology.numAccEnableC)
    }
  }

  private def systolicWireAccControl(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
    accControlSource: Vec[AccControl],
  ): Unit = {

    // One registered stage at the array boundary, in a buffer module rather than
    // loose flops — the accControl twin of preprocessorA / preprocessorB.
    val accControlBuffer = preprocessorAccControl.get
    accControlBuffer.io.input := accControlSource
    val registeredSource = accControlBuffer.io.output

    // One accControl per outer PE (cluster), NOT per output lane: numAccControl is
    // outerPeRow (ReuseA) / outerPeCol (ReuseB), and every VectorPE io.accControlInput
    // is size numOuterPeAccControl == 1. Under broadcast the lvR/lvC output lanes of a
    // cluster share the same drain timing, so the cluster carries a single accControl
    // and connectPostProcessorToOutput fans its accControlOutput(0) out to all lanes.
    // For lvR/lvC == 1 this is identical to a per-lane mapping.
    config.dataflow match {
      case Dataflow.ReuseA =>
        for (r <- 0 until config.outerPeRow)
          pes(r)(0).io.accControlInput(0) := registeredSource(r)

        for {
          r <- 0 until config.outerPeRow
          c <- 1 until config.outerPeCol
        } pes(r)(c).io.accControlInput(0) := pes(r)(c - 1).io.accControlOutput(0)

      case Dataflow.ReuseB =>
        for (c <- 0 until config.outerPeCol)
          pes(0)(c).io.accControlInput(0) := registeredSource(c)

        for {
          r <- 1 until config.outerPeRow
          c <- 0 until config.outerPeCol
        } pes(r)(c).io.accControlInput(0) := pes(r - 1)(c).io.accControlOutput(0)

      case Dataflow.ReuseC =>
        throw new IllegalArgumentException(
          "[error] Toroidal ReuseC does not need accumulator"
        )
    }
  }

  override def wireC(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    connectPeChain(pes)
    connectPostProcessorToOutput(pes)
  }

  /**
   * The C chain does NOT wrap — only the operands do. A partial sum has to terminate
   * somewhere for the array to have an output, so these are the same linear chains a
   * Grouped array uses.
   *
   * ReuseC reuses `connectPeChainReuseB` because its drain also runs north to south;
   * what it carries differs (finished output-stationary elements being shifted out,
   * rather than a sum growing along the way), but the wiring is identical.
   */
  override def connectPeChain(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        connectPeChainReuseA(pes)

      case Dataflow.ReuseB =>
        connectPeChainReuseB(pes)

      case Dataflow.ReuseC =>
        connectPeChainReuseB(pes)
    }
  }

  /**
   * Collect the array's results and hand them to the accumulator. There is no de-skew
   * buffer in this fabric (Grouped's `postProcessorC` has no counterpart here), so the
   * name is inherited from the base contract rather than describing a module.
   *
   * That absence is why `collectedAccControl` is built PER PORT here instead of being a
   * single shared copy: alignment is not created at the edge, it is inherited from the
   * fact that each port's control walked the same uniform number of hops to the output
   * edge. Each port therefore carries its own control out of its own last cluster, and
   * [[SystolicArray.attachAccumulatorIfNeeded]] takes port 0's as representative.
   */
  override def connectPostProcessorToOutput(pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {

    val collectedOutputC = Vec(portType.createSystolicOutputTypeC, topology.systolicOutputDimC)
    val collectedAccControl =
      if (config.needAccumulator) Vec(AccControl(), topology.systolicOutputDimC)
      else null

    config.dataflow match {
      case Dataflow.ReuseA =>
        if (dualModeTreeReuseA) {
          val chainOutputC = Vec(portType.createSystolicOutputTypeC, topology.systolicOutputDimC)
          collectReuseAOutputs(pes, chainOutputC)
          muxDualModeOutputs(pes, chainOutputC, collectedOutputC)
        } else {
          collectReuseAOutputs(pes, collectedOutputC)
        }
        if (config.needAccumulator) {
          for {
            r <- 0 until config.outerPeRow
            a <- 0 until config.localVectorPeRow
          } {
            val outputIndex = r * config.localVectorPeRow + a
            collectedAccControl(outputIndex) :=
              pes(r)(config.outerPeCol - 1).io.accControlOutput(0)
          }
        }

      case Dataflow.ReuseB =>
        if (dualModeTreeReuseB) {
          val chainOutputC = Vec(portType.createSystolicOutputTypeC, topology.systolicOutputDimC)
          collectReuseBOutputs(pes, chainOutputC)
          muxDualModeOutputs(pes, chainOutputC, collectedOutputC)
        } else {
          collectReuseBOutputs(pes, collectedOutputC)
        }
        if (config.needAccumulator) {
          for {
            c <- 0 until config.outerPeCol
            b <- 0 until config.localVectorPeCol
          } {
            val outputIndex = c * config.localVectorPeCol + b
            collectedAccControl(outputIndex) :=
              pes(config.outerPeRow - 1)(c).io.accControlOutput(0)
          }
        }

      case Dataflow.ReuseC =>
        // Each bottom-edge cluster drains its lvC output columns; the drained M rows
        // arrive over successive cycles on this totalVectorPeCol-wide bus.
        for {
          c <- 0 until config.outerPeCol
          b <- 0 until config.localVectorPeCol
        } {
          collectedOutputC(c * config.localVectorPeCol + b) :=
            pes(config.outerPeRow - 1)(c).io.outputC(b)
        }
    }

    attachAccumulatorIfNeeded(
      collected           = collectedOutputC,
      collectedAccControl = collectedAccControl,
      outputC             = io.outputC,
    )

  }

  /**
   * Dual-mode output stage. The second stream is consumed fully aligned (no per-PE
   * skew), so every VectorPE computes one GEMV element's products on the SAME cycle
   * and a [[DualModeAdderTree]] reduces their `outputPartial` taps along the
   * contraction axis — the stock C-chain carries stream-0 results only.
   *
   * Phase 1 keeps the port timing byte-compatible with the chain: the tree is
   * log2Up(chainHops) stages deep where the chain is `chainHops` registered hops
   * (one per outer PE along the contraction axis; the intra-cluster log2(lv) tree
   * stages exist identically on both paths), so a compensating delay of
   * chainHops - log2Up(chainHops) re-aligns it. `accControl`, the AccSram path and
   * every driver latency constant stay valid unchanged; phase 2 deletes the
   * compensation and retimes them to reclaim those cycles.
   *
   * The stream select, delayed by the dual path's total depth (mux-at-MAC ->
   * mult(1) -> PE tree log2(multPerPe) -> array tree -> compensation), time-demuxes
   * the shared output between chain and tree — the successor of the old
   * per-PE-skew `selectOut` contract, with the same timing.
   *
   * Only called when the contraction axis spans MORE than one outer PE
   * (dualModeTreeReuseA/B). With a single outer hop the intra-cluster adder tree
   * already reduces the whole axis and the chain output IS the dual result, so
   * building this tree would just duplicate it value- and timing-identically.
   */
  private def muxDualModeOutputs(
    pes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]],
    chainOutputC: Vec[AccType],
    collectedOutputC: Vec[AccType],
  ): Unit = {

    val tree = new DualModeAdderTree[InputType, AccType](config)

    // Tap collection, lane-major: lane = physical column (ReuseB) / row (ReuseA),
    // k walks the contraction axis physical-major. Standalone configs degenerate to
    // outputPartial(0) per PE.
    if (config.isReuseB) {
      for {
        c <- 0 until config.outerPeCol
        b <- 0 until config.localVectorPeCol
        r <- 0 until config.outerPeRow
        a <- 0 until config.localVectorPeRow
      } {
        val lane = c * config.localVectorPeCol + b
        val k    = r * config.localVectorPeRow + a
        tree.io.input(lane * tree.tapsPerLane + k) :=
          pes(r)(c).io.outputPartial(a * config.localVectorPeCol + b)
      }
    } else {
      for {
        r <- 0 until config.outerPeRow
        a <- 0 until config.localVectorPeRow
        c <- 0 until config.outerPeCol
        b <- 0 until config.localVectorPeCol
      } {
        val lane = r * config.localVectorPeRow + a
        val k    = c * config.localVectorPeCol + b
        tree.io.input(lane * tree.tapsPerLane + k) :=
          pes(r)(c).io.outputPartial(a * config.localVectorPeCol + b)
      }
    }

    val chainHops    = if (config.isReuseB) config.outerPeRow else config.outerPeCol
    val compensation = chainHops - log2Up(chainHops)

    val zero = portType.zeroSystolicOutputTypeC
    val delayedTree = Vec(tree.io.output.map(o => Delay(o, compensation, init = zero)))

    // The BUFFERED select, i.e. the one the MACs actually see — the port's own extra
    // cycle is shared by the second stream, so the two stay aligned.
    val select        = selectBuffer.get.io.output(StreamSelectIdx)
    val dualPathDepth = 1 + topology.log2MultPerPe + log2Up(tree.tapsPerLane) + compensation
    val selectDelayed = Delay(select, dualPathDepth, init = False)

    for (i <- 0 until topology.systolicOutputDimC)
      collectedOutputC(i) := Mux(selectDelayed, delayedTree(i), chainOutputC(i))
  }

}