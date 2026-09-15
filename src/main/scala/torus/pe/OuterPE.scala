package torus.pe

import spinal.core._

import torus.accumulator._
import torus.arithmetic._
import torus.config._
import torus.core._
import torus.topology._

/**
 * A CLUSTER of VectorPEs presented to the array as one PE — the middle level of the
 * hierarchy, and the shared half of [[GroupPE]] (Grouped fabric) and [[ClusterPE]]
 * (Toroidal fabric).
 *
 * The level is OPTIONAL. When `localVectorPeRow == localVectorPeCol == 1` the array
 * instantiates VectorPEs directly and nothing here is built at all; this trait exists
 * only for `lvR × lvC > 1`. That is why so much of the project asks
 * `isStandAloneVectorPeConfig` — it is asking which of the two shapes the fabric has.
 *
 * What the trait provides is an IO bundle plus a LIBRARY of wiring primitives. It wires
 * nothing itself: `wire()` fixes the order (A, B, control, then C, since `wireC` reads
 * outputs the others created) and leaves the four steps abstract, so each subclass
 * matches on `config.dataflow` and picks primitives from below. Read a subclass as a
 * 3×4 table of those choices.
 *
 * The primitives come in two flavours, and choosing between them IS the dataflow:
 *
 *  - `systolicWireX` — chain: PE `b` takes its operand from PE `b-1`'s registered
 *    output, so each PE sees a different element, one hop per cycle. Used for the
 *    STATIONARY operand, whose per-PE values have to be walked into place.
 *  - `broadcastWireX` — fan-out: every PE on the axis takes the cluster's input
 *    directly, combinationally, and the cluster forwards it with exactly ONE register.
 *    Used where the whole axis shares a value. The single register is load-bearing: it
 *    keeps the cluster's external skew at 1 cycle, the same as a lone VectorPE's, so the
 *    array's skew buffers do not need to know whether clusters are composite.
 *
 * `rowWiseAccumulate` / `colWiseAccumulate` are the C-side pair, reducing the cluster's
 * VectorPE outputs along whichever axis is the contraction axis (rows for ReuseA,
 * columns for ReuseB) and adding the upstream cluster's `inputC` when this cluster has
 * one. Both register the result, which is the cluster's one hop of the C-chain.
 *
 * Widths and port existence are never decided here — they come from
 * `PortTypeProvider` and the [[torus.core.PortEnableMask]] the topology handed down, both
 * keyed on `outerPeIndex`, so an edge cluster simply has fewer ports.
 */
trait OuterPE[InputType <: Data, AccType <: Data] extends ProcessingElementIO[InputType, AccType] with AdderTreeSupport[AccType] {

  def config: SystolicArrayConfig
  def portMask: PortEnableMask
  def outerPeIndex: OuterPeIndex
  final protected def topology: ArrayTopology = config.topology

  def dualMode: Boolean = false

  final protected def dualModeReuseA: Boolean = dualMode && config.isReuseA
  final protected def dualModeReuseB: Boolean = dualMode && config.isReuseB

  final protected def dualModeInputDim: Int =
    config.localVectorPeRow * config.localVectorPeCol * config.multPerPe

  // The dual-mode tap ports exist only when the contraction axis spans more than one
  // outer PE; with a single hop the intra-cluster tree has already reduced it. Must stay
  // identical to VectorPE's predicate of the same name — one side generating the port
  // and the other not is an elaboration-time null, not a width mismatch.
  final protected def dualModeTree: Boolean =
    (dualModeReuseB && config.outerPeRow > 1) || (dualModeReuseA && config.outerPeCol > 1)

  override val io = new Bundle {

    val inputA = in Vec(portType.createInputTypeA, topology.outerPeInputDimA)
    val inputB = in Vec(portType.createInputTypeB, topology.outerPeInputDimB)
    val inputC = in Vec(portType.createOuterPeInputTypeC(outerPeIndex), topology.outerPeDimC)

    val inputA2: Vec[InputType] = dualModeReuseB generate { in Vec(portType.createInputTypeA, dualModeInputDim) }
    val selectA: Bool = dualModeReuseB generate { in Bool() }

    val inputB2: Vec[InputType] = dualModeReuseA generate { in Vec(portType.createInputTypeB, dualModeInputDim) }
    val selectB: Bool = dualModeReuseA generate { in Bool() }

    val inputUpdateEnableA = config.isReuseA generate {in Vec(Bool(), topology.numOuterPeUpdateEnableA)}
    val inputUpdateEnableB = config.isReuseB generate {in Vec(Bool(), topology.numOuterPeUpdateEnableB)}
    val inputAccEnableC = config.isReuseC generate {in Vec(Bool(), topology.numOuterPeAccEnableC)}
    val accControlInput = topology.needClusterPeAccControlPort(outerPeIndex) generate { in Vec(AccControl(), topology.numOuterPeAccControl)}

    val outputA = portMask.withOutputPortA generate {
      out Vec(portType.createInputTypeA, topology.outerPeOutputDimA(outerPeIndex))
    }
    val outputB = portMask.withOutputPortB generate {
      out Vec(portType.createInputTypeB, topology.outerPeOutputDimB(outerPeIndex))
    }

    val outputC = out Vec(portType.createOuterPeOutputTypeC(outerPeIndex), topology.outerPeDimC)

    val outputPartial: Vec[AccType] = dualModeTree generate {
      out Vec(portType.createVectorPeDotProductType, config.localVectorPeRow * config.localVectorPeCol)
    }

    val outputUpdateEnableA = (config.isReuseA && portMask.withOutputPortA) generate {
      out Vec(Bool(), topology.numOuterPeUpdateEnableA)
    }
    val outputUpdateEnableB = (config.isReuseB && portMask.withOutputPortB) generate {
      out Vec(Bool(), topology.numOuterPeUpdateEnableB)
    }
    val outputAccEnableC = (config.isReuseC && config.topology.needAccEnableC(outerPeIndex)) generate {
      out Vec(Bool(), topology.numOuterPeAccEnableC)
    }
    val accControlOutput = topology.needClusterPeAccControlPort(outerPeIndex) generate { out Vec(AccControl(), topology.numOuterPeAccControl)}

  }

  implicit val arithmetic: Arithmetic[InputType, AccType]
  implicit def portType: PortTypeProvider[InputType, AccType]

  // Fixed order, abstract steps: `wireC` reads PE outputs the earlier steps created, so
  // the sequence is the trait's to own and only the CONTENT of each step is per-subclass.
  protected final def wire(): Unit = {
    wireA()
    wireB()
    wireControl()
    wireC()
  }
  protected def wireA(): Unit
  protected def wireB(): Unit
  protected def wireControl(): Unit
  protected def wireC(): Unit

  protected final def createVectorPes(): Vector[Vector[VectorPE[InputType, AccType]]] = {
    Vector.tabulate(config.localVectorPeRow, config.localVectorPeCol) { (a, b) =>
      new VectorPE(
        outerIndex = outerPeIndex,
        innerIndex = InnerIndex(a, b),
        vectorPortMask = config.topology.innerVectorPePortMask(portMask, a, b),
        config = config,
        dualMode = dualMode,
      )
    }
  }

  protected final def systolicWireA(vectorPes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    for {
      a <- 0 until config.localVectorPeRow
      p <- 0 until config.multPerPe
    } vectorPes(a)(0).io.inputA(p) := io.inputA(a * config.multPerPe + p)

    for {
      a <- 0 until config.localVectorPeRow
      b <- 1 until config.localVectorPeCol
      p <- 0 until config.multPerPe
    } vectorPes(a)(b).io.inputA(p) := vectorPes(a)(b - 1).io.outputA(p)

    if(portMask.withOutputPortA)
      for {
        a <- 0 until config.localVectorPeRow
        p <- 0 until config.multPerPe
      } io.outputA(a * config.multPerPe + p) := vectorPes(a)(config.localVectorPeCol - 1).io.outputA(p)

  }

  // Broadcast A across the cluster's columns: every column b of sub-row a sees the same
  // element, so the fan-out is combinational and only the FORWARD path is registered.
  // That single register is what keeps a composite cluster's external skew equal to a
  // lone VectorPE's — the array's skew buffers are sized for 1 cycle per hop either way.
  protected final def broadcastWireA(vectorPes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    for {
      a <- 0 until config.localVectorPeRow
      b <- 0 until config.localVectorPeCol
      p <- 0 until config.multPerPe
    } vectorPes(a)(b).io.inputA(p) := io.inputA(a * config.multPerPe + p)

    if(portMask.withOutputPortA)
      io.outputA := RegNext(io.inputA, init = Vec.fill(topology.outerPeInputDimA)(portType.zeroInputA))
  }

  protected final def systolicWireB(vectorPes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    for {
      b <- 0 until config.localVectorPeCol
      p <- 0 until config.multPerPe
    } vectorPes(0)(b).io.inputB(p) := io.inputB(b * config.multPerPe + p)

    for {
      a <- 1 until config.localVectorPeRow
      b <- 0 until config.localVectorPeCol
      p <- 0 until config.multPerPe
    } vectorPes(a)(b).io.inputB(p) := vectorPes(a - 1)(b).io.outputB(p)

    if(portMask.withOutputPortB)
      for {
        b <- 0 until config.localVectorPeCol
        p <- 0 until config.multPerPe
      } io.outputB(b * config.multPerPe + p) := vectorPes(config.localVectorPeRow - 1)(b).io.outputB(p)


  }

  // The transpose of broadcastWireA: shared down the rows, indexed by column.
  protected final def broadcastWireB(vectorPes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    for {
      a <- 0 until config.localVectorPeRow
      b <- 0 until config.localVectorPeCol
      p <- 0 until config.multPerPe
    } vectorPes(a)(b).io.inputB(p) := io.inputB(b * config.multPerPe + p)

    if (portMask.withOutputPortB) {
      io.outputB := RegNext(io.inputB, init = Vec.fill(topology.outerPeInputDimB)(portType.zeroInputB))
    }

  }

  protected final def systolicUpdateEnableA(vectorPes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    for (a <- 0 until config.localVectorPeRow)
      vectorPes(a)(0).io.inputUpdateEnableA(0) := io.inputUpdateEnableA(a)

    for {
      a <- 0 until config.localVectorPeRow
      b <- 1 until config.localVectorPeCol
    } {
      vectorPes(a)(b).io.inputUpdateEnableA(0) := vectorPes(a)(b-1).io.outputUpdateEnableA(0)
    }

    if(portMask.withOutputPortA)
      for(a <- 0 until config.localVectorPeRow)
        io.outputUpdateEnableA(a) := vectorPes(a)(config.localVectorPeCol - 1).io.outputUpdateEnableA(0)

  }

  protected final def systolicUpdateEnableB(vectorPes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]]): Unit = {
    for ( b <- 0 until config.localVectorPeCol)
      vectorPes(0)(b).io.inputUpdateEnableB(0) := io.inputUpdateEnableB(b)

    for {
      a <- 1 until config.localVectorPeRow
      b <- 0 until config.localVectorPeCol
    } {
      vectorPes(a)(b).io.inputUpdateEnableB(0) := vectorPes(a-1)(b).io.outputUpdateEnableB(0)
    }

    if(portMask.withOutputPortB)
      for( b <- 0 until config.localVectorPeCol)
        io.outputUpdateEnableB(b) := vectorPes(config.localVectorPeRow-1)(b).io.outputUpdateEnableB(0)

  }

  // ReuseA's C-side reduction: sum each sub-ROW's VectorPE outputs (the contraction runs
  // along b), add the upstream cluster's partial when this cluster has an inputC, and
  // register — that register is the cluster's single hop of the array's C-chain. The
  // resize to the declared output type is the only place the tree's expanding adds are
  // brought back to a port width. colWiseAccumulate is the ReuseB transpose.
  protected final def rowWiseAccumulate(vectorPes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]], outerIndex: OuterPeIndex): Unit = {
    import arithmetic._

    val allRowSums = Vec.tabulate(config.localVectorPeRow) { a =>
      val rowOutputs = Vec.tabulate(config.localVectorPeCol) { b =>
        vectorPes(a)(b).io.outputC(0)
      }
      adderTree(rowOutputs)
    }

    val outputType = portType.createOuterPeOutputTypeC(outerIndex)
    val zero = portType.zeroOuterPeOutputTypeC(outerIndex)

    for(a <- 0 until config.localVectorPeRow){
      if(portMask.withInputPortC){
        io.outputC(a) := RegNext((allRowSums(a) + io.inputC(a)).resizeTo(outputType), init = zero)
      } else {
        io.outputC(a) := RegNext(allRowSums(a).resizeTo(outputType), init = zero)
      }
    }

  }

  protected final def colWiseAccumulate(vectorPes: Vector[Vector[Component with ProcessingElementIO[InputType, AccType]]], outerIndex: OuterPeIndex): Unit = {
    import arithmetic._

    val allColSums = Vec.tabulate(config.localVectorPeCol){ b =>
      val rowOutputs = Vec.tabulate(config.localVectorPeRow) { a =>
        vectorPes(a)(b).io.outputC(0)
      }
      adderTree(rowOutputs)
    }

    val outputType = portType.createOuterPeOutputTypeC(outerIndex)
    val zero = portType.zeroOuterPeOutputTypeC(outerIndex)

    for(b <- 0 until config.localVectorPeCol){
      if(portMask.withInputPortC){
        io.outputC(b) := RegNext((allColSums(b) + io.inputC(b)).resizeTo(outputType), init = zero)
      } else {
        io.outputC(b) := RegNext(allColSums(b).resizeTo(outputType), init = zero)

      }
    }



  }
}
