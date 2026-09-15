package torus.pe

import spinal.core._
import spinal.lib.Delay

import torus.accumulator._
import torus.arithmetic._
import torus.config._
import torus.core._

/**
 * The [[OuterPE]] of the TOROIDAL fabric, and the only PE that carries dual mode.
 *
 * Same 3×4 shape as [[GroupPE]] — one operand chained and the other broadcast for
 * ReuseA/ReuseB, both broadcast for ReuseC — but it differs from its Grouped twin in
 * three ways, and each is a consequence of the wrap:
 *
 *  1. The NON-stationary operand is forwarded with a split register. Rather than
 *     registering the whole forward Vec, the lanes below the last sub-row (`ReuseB`) or
 *     sub-column (`ReuseA`) pass through combinationally and only the last group is
 *     registered. Those early lanes have already taken their cycle of skew crossing the
 *     cluster's own PEs; registering them again would double-count it.
 *  2. ReuseC's edge clusters forward A and B UNREGISTERED. The rightmost / bottom
 *     cluster feeds the wrap buffer, not another cluster, and the wrap's own latency is
 *     what closes the loop — an extra register here would shift the recirculated
 *     operand off its partner by one cycle.
 *  3. ReuseC aligns `accEnableC` by GLOBAL PHYSICAL ROW instead of by
 *     `topology.additionalDelay`. This is why the class does not mix in
 *     [[AccEnableDelaySupport]] as GroupPE does: a broadcast cluster has several physical
 *     rows behind one outer index, which `additionalDelay` cannot express. The array
 *     pre-delays the gate by `r * lvR`, this class adds `a` per inner sub-row, and emits
 *     the cluster's output gate delayed by the full `lvR`, keeping one cycle per physical
 *     row across the whole chain. `VectorPE.buildReuseC` completes the alignment.
 *
 * Dual mode is a pure PASS-THROUGH here. The second stream's lanes are sliced per inner
 * VectorPE and handed down (`wireDualModeA` / `wireDualModeB`), and the resulting dot
 * products are forwarded up untouched (`wirePartialTaps`). The mux between the two
 * streams lives at the MAC inside [[VectorPE]], and the reduction lives in the
 * array-level [[DualModeAdderTree]]; this level only routes. ReuseA and ReuseB are exact
 * mirrors — whichever operand streams is the one that gets the second lane.
 *
 * `accControl` (the accumulator's handshake) is not data and does not follow the
 * datapath: `wireAccControlBy` either routes it through the cluster's source VectorPE
 * with a delay matching the intra-cluster tree depth, or, for a cluster that merely sits
 * on the path, forwards it with a single register.
 */
class ClusterPE[InputType <: Data, AccType <: Data](
  val outerPeIndex: OuterPeIndex,
  val portMask: PortEnableMask,
  val config: SystolicArrayConfig,
  override val dualMode: Boolean = false,
)(
  implicit val arithmetic: Arithmetic[InputType, AccType],
  implicit val portType: PortTypeProvider[InputType, AccType],
) extends Component
  with OuterPE[InputType, AccType]
  with AdderTreeSupport[AccType]{

  require(config.isClusterPeConfig, "[error] Only Toroidal configurations can call Cluster PE")

  private val vectorPes = createVectorPes()
  wire()
  if (dualModeTree) wirePartialTaps()

  // Forward each inner VectorPE's dual-mode dot-product tap upward, VectorPE-major —
  // pure pass-through, the array-level DualModeAdderTree does the reduction.
  private def wirePartialTaps(): Unit = {
    for {
      a <- 0 until config.localVectorPeRow
      b <- 0 until config.localVectorPeCol
    } io.outputPartial(a * config.localVectorPeCol + b) := vectorPes(a)(b).io.outputPartial(0)
  }

  def wireA(): Unit = {

    config.dataflow match {
      case Dataflow.ReuseA =>
        systolicWireA(vectorPes)

      case Dataflow.ReuseB =>
        // A streams. Broadcast it across the cluster's columns, then forward with a
        // SPLIT register: lanes below the last sub-row pass through combinationally
        // because they already took their cycle of skew crossing this cluster's PEs,
        // and only the last sub-row's lanes are registered. Registering all of them
        // would double-count that skew against the neighbouring cluster.
        val lastRowPe = config.localVectorPeRow - 1
        val lastRowStartIndex = lastRowPe * config.multPerPe

        for {
          a <- 0 until config.localVectorPeRow
          b <- 0 until config.localVectorPeCol
          p <- 0 until config.multPerPe
        } {
          vectorPes(a)(b).io.inputA(p) := io.inputA(a * config.multPerPe + p)
        }

        val outputDim = topology.outerPeOutputDimA(outerPeIndex)
        for (i <- 0 until outputDim) {
          if (i < lastRowStartIndex) {
            io.outputA(i) := io.inputA(i)
          } else {
            io.outputA(i) := RegNext(io.inputA(i), portType.zeroInputA)
          }
        }


      case Dataflow.ReuseC =>
        // Output-stationary: broadcast A for sub-row a down the cluster's columns
        // (every column b holds a different weight but shares row a's activation),
        // then forward the whole row-group to the next column cluster with the same
        // uniform 1-cycle systolic skew the standalone VectorPE uses — register every
        // lane, except the rightmost cluster which passes A straight to the wrap buffer.
        for {
          a <- 0 until config.localVectorPeRow
          b <- 0 until config.localVectorPeCol
          p <- 0 until config.multPerPe
        } {
          vectorPes(a)(b).io.inputA(p) := io.inputA(a * config.multPerPe + p)
        }

        if (portMask.withOutputPortA) {
          if (outerPeIndex.c == config.outerPeCol - 1) {
            io.outputA := io.inputA
          } else {
            io.outputA := RegNext(
              io.inputA,
              init = Vec.fill(topology.outerPeInputDimA)(portType.zeroInputA),
            )
          }
        }

    }

    if (dualModeReuseB) wireDualModeA()

  }

  // Fan the per-VectorPE dual-mode lanes (io.inputA2, VectorPE-major) straight to
  // each inner VectorPE's dual-mode input, and broadcast the shared MAC select. The
  // actual inputA/inputA2 mux lives inside VectorPE, at the MAC. ReuseB only —
  // wireDualModeB is the ReuseA mirror.
  private def wireDualModeA(): Unit = {
    for {
      a <- 0 until config.localVectorPeRow
      b <- 0 until config.localVectorPeCol
    } {
      val base = (a * config.localVectorPeCol + b) * config.multPerPe
      for (p <- 0 until config.multPerPe)
        vectorPes(a)(b).io.inputA2(p) := io.inputA2(base + p)
      vectorPes(a)(b).io.selectA := io.selectA
    }
  }

  // ReuseA mirror of wireDualModeA: same VectorPE-major slicing, on the B lanes.
  private def wireDualModeB(): Unit = {
    for {
      a <- 0 until config.localVectorPeRow
      b <- 0 until config.localVectorPeCol
    } {
      val base = (a * config.localVectorPeCol + b) * config.multPerPe
      for (p <- 0 until config.multPerPe)
        vectorPes(a)(b).io.inputB2(p) := io.inputB2(base + p)
      vectorPes(a)(b).io.selectB := io.selectB
    }
  }

  def wireB(): Unit = {

    config.dataflow match {
      case Dataflow.ReuseA =>
        // B streams here — the transpose of wireA's ReuseB branch, same split register
        // on the column axis.
        val lastColPe = config.localVectorPeCol - 1
        val lastColStartIndex = lastColPe * config.multPerPe

        for {
          a <- 0 until config.localVectorPeRow
          b <- 0 until config.localVectorPeCol
          p <- 0 until config.multPerPe
        } {
          vectorPes(a)(b).io.inputB(p) := io.inputB(b * config.multPerPe + p)
        }

        val outputDim = topology.outerPeOutputDimB(outerPeIndex)
        for (i <- 0 until outputDim) {
          if( i < lastColStartIndex){
            io.outputB(i) := io.inputB(i)
          } else {
            io.outputB(i) := RegNext(io.inputB(i), portType.zeroInputB)
          }
        }


      case Dataflow.ReuseB =>
        systolicWireB(vectorPes)

      case Dataflow.ReuseC =>
        // Symmetric to wireA: broadcast B for sub-col b across the cluster's rows,
        // then forward the column-group down to the next row cluster with a uniform
        // 1-cycle skew — register every lane, except the bottom cluster which passes
        // B straight to the wrap buffer.
        for {
          a <- 0 until config.localVectorPeRow
          b <- 0 until config.localVectorPeCol
          p <- 0 until config.multPerPe
        } {
          vectorPes(a)(b).io.inputB(p) := io.inputB(b * config.multPerPe + p)
        }

        if (portMask.withOutputPortB) {
          if (outerPeIndex.r == config.outerPeRow - 1) {
            io.outputB := io.inputB
          } else {
            io.outputB := RegNext(
              io.inputB,
              init = Vec.fill(topology.outerPeInputDimB)(portType.zeroInputB),
            )
          }
        }

    }

    if (dualModeReuseA) wireDualModeB()

  }

  def wireControl(): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        systolicUpdateEnableA(vectorPes)
        if(topology.needClusterPeAccControlPort(outerPeIndex)){
          wireAccControlBy(
            sourceVectorPe = _ => vectorPes(0)(0),
            delayCycles    = 1 + topology.log2LocalVectorPeCol,
          )
        }

      case Dataflow.ReuseB =>
        systolicUpdateEnableB(vectorPes)
        if(topology.needClusterPeAccControlPort(outerPeIndex)){
          wireAccControlBy(
            sourceVectorPe = _ => vectorPes(0)(0),
            delayCycles    = 1 + topology.log2LocalVectorPeRow,
          )
        }

      case Dataflow.ReuseC =>
        // accEnableC drives the output-stationary accumulate-then-drain gate. Each
        // physical row must see it delayed by its global-row depth so every row's
        // drain aligns (see VectorPE.buildReuseC). The array feeds this cluster the
        // gate already delayed by r*lvR; here we add the intra-cluster `a` cycles per
        // sub-row, and emit the cluster's output gate delayed by the full lvR so the
        // next row cluster continues the 1-per-physical-row chain.
        for {
          a <- 0 until config.localVectorPeRow
          b <- 0 until config.localVectorPeCol
        } {
          vectorPes(a)(b).io.inputAccEnableC(0) :=
            Delay(io.inputAccEnableC(b), a, init = False)
        }

        if (topology.needAccEnableC(outerPeIndex)) {
          for (b <- 0 until config.localVectorPeCol) {
            io.outputAccEnableC(b) :=
              Delay(io.inputAccEnableC(b), config.localVectorPeRow, init = False)
          }
        }
    }
  }

  // The accumulator handshake is control, not data, so it does not ride the datapath.
  // A cluster that CONTAINS the control source routes it through that VectorPE and then
  // delays the answer by the intra-cluster tree depth, so the handshake leaves in step
  // with the sum it describes. A cluster that merely sits on the path forwards it with
  // one register, matching the one hop per cluster the datapath takes.
  private def wireAccControlBy(
    sourceVectorPe: Int => Component with ProcessingElementIO[InputType, AccType],
    delayCycles: Int,
  ): Unit = {
    if (topology.isAccControlSourceIncluded(outerPeIndex)) {
      for (i <- 0 until topology.numOuterPeAccControl) {
        sourceVectorPe(i).io.accControlInput(0) := io.accControlInput(i)
      }
      for (i <- 0 until topology.numOuterPeAccControl) {
        io.accControlOutput(i) := Delay(
          sourceVectorPe(i).io.accControlOutput(0),
          delayCycles,
          init = AccControl.zero
        )
      }
    } else if (topology.isControlSourceCarrier(outerPeIndex)) {
      io.accControlOutput := RegNext(
        io.accControlInput,
        init = Vec.fill(topology.numOuterPeAccControl)(AccControl.zero)
      )
    }
  }

  def wireC(): Unit = {
    config.dataflow match {
      case Dataflow.ReuseA =>
        rowWiseAccumulate(vectorPes, outerPeIndex)

      case Dataflow.ReuseB =>
        colWiseAccumulate(vectorPes, outerPeIndex)

      case Dataflow.ReuseC =>
        // Chain the inner sub-rows' C ports into one drain column per cluster column b:
        // sub-row a>0 takes its upstream partial from the sub-row above; sub-row 0 takes
        // it from the cluster's inputC (the cluster above) when present. The bottom
        // sub-row drives the cluster's outputC, which the array chains to the cluster
        // below and ultimately reads at the array's bottom edge.
        for {
          a <- 1 until config.localVectorPeRow
          b <- 0 until config.localVectorPeCol
        } {
          vectorPes(a)(b).io.inputC(0) := vectorPes(a - 1)(b).io.outputC(0)
        }

        if (portMask.withInputPortC) {
          for (b <- 0 until config.localVectorPeCol)
            vectorPes(0)(b).io.inputC(0) := io.inputC(b)
        }

        for (b <- 0 until config.localVectorPeCol)
          io.outputC(b) := vectorPes(config.localVectorPeRow - 1)(b).io.outputC(0)

    }
  }


}
