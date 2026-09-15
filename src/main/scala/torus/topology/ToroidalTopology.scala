package torus.topology

import torus.config._
import torus.core._

abstract class ToroidalTopology(val toroidal: ToroidalConfig) extends ArrayTopology {

  override def config: SystolicArrayConfig = toroidal

  // ════════════════════════════════════════════════════════════
  //  Grouped-only quantities — Toroidal has none of these.
  // ════════════════════════════════════════════════════════════

  private def groupedOnly(name: String): Nothing =
    throw new IllegalArgumentException(s"[error] $name is Grouped-only")

  override def numVectorPePerGroupPe: Int =
    groupedOnly("numVectorPePerGroupPe")

  override def skewBufferDepth(tileType: TileType.Value): Int =
    groupedOnly(s"skewBufferDepth($tileType)")
  override def skewBufferStepA: Int = groupedOnly("skewBufferStepA")
  override def skewBufferStepB: Int = groupedOnly("skewBufferStepB")

  override def skewBufferPortsPerStep(tileType: TileType.Value): Int =
    groupedOnly(s"skewBufferPortsPerStep($tileType)")
  override def skewBufferPortsPerStepA: Int = groupedOnly("skewBufferPortsPerStepA")
  override def skewBufferPortsPerStepB: Int = groupedOnly("skewBufferPortsPerStepB")

  override def postBufferStep: Int =
    groupedOnly("postBufferStep")
  override def postBufferPortsPerStep: Int =
    groupedOnly("postBufferPortsPerStep")

  override def numAccTapCellPort: Int =
    groupedOnly("numAccTapCellPort")

  override def accTapCellDepth: Int =
    groupedOnly("accTapCellDepth")

  override def accTapPreludeDelay: Int =
    groupedOnly("accTapPreludeDelay")


  //systolic arrays
  override def numUpdateEnableA: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  override def numUpdateEnableB: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  override def numAccEnableC: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")


  //Cluster PE
  override def numOuterPeUpdateEnableA: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  override def numOuterPeUpdateEnableB: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  override def numOuterPeAccEnableC: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  //Vector PE
  override def additionalDelay(outerPeIndex: OuterPeIndex): Int =
    throw new IllegalArgumentException(s"[error] Toroidal ReuseA/B cannot use this")

  //buffers
  override def wrapBufferCellPort: Int =
    throw new IllegalArgumentException("[error] WrapBuffer is Toroidal ReuseC only")

  override def wrapBufferNumCell(tileType: TileType.Value): Int =
    throw new IllegalArgumentException("[error] WrapBuffer is Toroidal ReuseC only")

  //acc
  override def needVectorPeAccControlPort(outerPeIndex: OuterPeIndex, innerIndex: InnerIndex): Boolean = {
    if(config.isReuseAB){
      isVectorPeAccControlSource(outerPeIndex, innerIndex) || isStandAloneAccControlCarrier(outerPeIndex)
    } else {
      false
    }
  }

  override def needClusterPeAccControlPort(outerPeIndex: OuterPeIndex): Boolean = {
    if(config.isReuseAB){
      isAccControlSourceIncluded(outerPeIndex) || isControlSourceCarrier(outerPeIndex)
    } else {
      false
    }
  }

}

class ToroidalReuseATopology(toroidal: ToroidalConfig) extends ToroidalTopology(toroidal) {

  //Systolic array dimensions

  override val systolicInputDimA: Int = toroidal.systolicArrayRowDimension
  // ReuseA streams B into the preprocessor, which wireB reads at output(r*mult+i)
  // for i<mult, so it needs outerPeRow*multPerPe ports (the lvR broadcast columns
  // do NOT add boundary ports). Equals systolicArrayColDimension via the ReuseA
  // constraint outerPeCol*lvC==outerPeRow.
  override val systolicInputDimB: Int = toroidal.outerPeRow * toroidal.multPerPe

  override val numUpdateEnableA: Int = toroidal.totalVectorPeRow

  override val systolicOutputDimC: Int = toroidal.totalVectorPeRow

  //Cluster PE
  override def outerPePortMask(outerPeIndex: OuterPeIndex): PortEnableMask = {
    PortEnableMask(
      withOutputPortA = outerPeIndex.c < toroidal.outerPeCol - 1,
      withOutputPortB = true,
      withInputPortC  = outerPeIndex.c > 0,
    )
  }

  override val outerPeInputDimA: Int = toroidal.localVectorPeRow * toroidal.multPerPe
//  override val outerPeInputDimB: Int = toroidal.clusterPeDiagonalInputDim
  override val outerPeInputDimB: Int = toroidal.localVectorPeCol * toroidal.multPerPe
  override val outerPeDimC: Int = toroidal.localVectorPeRow

  override val numOuterPeUpdateEnableA: Int = toroidal.localVectorPeRow
  override def needAccEnableC(outerPeIndex: OuterPeIndex): Boolean = false

  override def outerPeOutputDimA(outerPeIndex: OuterPeIndex): Int = outerPeInputDimA
  override def outerPeOutputDimB(outerPeIndex: OuterPeIndex): Int =
    if (outerPeIndex.c == toroidal.outerPeCol - 1) outerPeInputDimB - toroidal.multPerPe
    else outerPeInputDimB

  //Vector PE
  override def standAloneVectorPePortMask(r: Int, c: Int): PortEnableMask = {
    toroidal.requireStandAlone()
    PortEnableMask(
      withOutputPortA = c < toroidal.outerPeCol - 1,
      withOutputPortB = c < toroidal.outerPeCol - 1,
      withInputPortC  = c > 0,
    )
  }

  override def innerVectorPePortMask(
    outerPePortMask: PortEnableMask,
    a: Int,
    b: Int,
  ): PortEnableMask = PortEnableMask(
    withOutputPortA = outerPePortMask.withOutputPortA || b < toroidal.localVectorPeCol - 1,
    withOutputPortB = false,
    withInputPortC  = false,
  )

  //acc
  override def numAccControl: Int = config.outerPeRow
  override def numOuterPeAccControl: Int = 1

  override def isVectorPeAccControlSource(outerPeIndex: OuterPeIndex, innerIndex: InnerIndex): Boolean = {
    if(config.needAccumulator) {
      if(config.isStandAloneVectorPeConfig){
        outerPeIndex.c == 0
      } else {
        innerIndex.b == 0 && outerPeIndex.c == 0
      }
    } else {
      false
    }
  }

  override def isStandAloneAccControlCarrier(outerPeIndex: OuterPeIndex): Boolean = {
    if (config.needAccumulator && config.isStandAloneVectorPeConfig) {
      outerPeIndex.c != 0
    } else {
      false
    }
  }

  override def isAccControlSourceIncluded(outerPeIndex: OuterPeIndex): Boolean = {
    if(config.needAccumulator && !config.isStandAloneVectorPeConfig){
      outerPeIndex.c == 0
    } else {
      false
    }

  }

  override def isControlSourceCarrier(outerPeIndex: OuterPeIndex): Boolean = {
    if(config.needAccumulator && !config.isStandAloneVectorPeConfig){
      outerPeIndex.c != 0
    } else {
      false
    }
  }

}

class ToroidalReuseBTopology(toroidal: ToroidalConfig) extends ToroidalTopology(toroidal) {

  //Systolic Arrays
  // ReuseB streams A into the preprocessor, which wireA reads at output(c*mult+i)
  // for i<mult, so it needs outerPeCol*multPerPe ports (the lvC broadcast columns
  // do NOT add boundary ports). Equals systolicArrayRowDimension via the ReuseB
  // constraint outerPeRow*lvR==outerPeCol.
  override val systolicInputDimA: Int = toroidal.outerPeCol * toroidal.multPerPe
  override val systolicInputDimB: Int = toroidal.systolicArrayColDimension

  override val numUpdateEnableB: Int = toroidal.totalVectorPeCol

  override val systolicOutputDimC: Int = toroidal.totalVectorPeCol

  //Cluster PE
  override def outerPePortMask(outerPeIndex: OuterPeIndex): PortEnableMask = {
    PortEnableMask(
      withOutputPortA = true,
      withOutputPortB = outerPeIndex.r < toroidal.outerPeRow - 1,
      withInputPortC  = outerPeIndex.r > 0,
    )
  }

//  override val outerPeInputDimA: Int = toroidal.clusterPeDiagonalInputDim
  override val outerPeInputDimA: Int = toroidal.localVectorPeRow * toroidal.multPerPe
  override val outerPeInputDimB: Int = toroidal.localVectorPeCol * toroidal.multPerPe
  override val outerPeDimC: Int = toroidal.localVectorPeCol

  override val numOuterPeUpdateEnableB: Int = toroidal.localVectorPeCol

  override def outerPeOutputDimA(outerPeIndex: OuterPeIndex): Int =
    if (outerPeIndex.r == toroidal.outerPeRow - 1) outerPeInputDimA - toroidal.multPerPe
    else outerPeInputDimA

  override def outerPeOutputDimB(outerPeIndex: OuterPeIndex): Int = outerPeInputDimB

  //Vector PE
  override def standAloneVectorPePortMask(r: Int, c: Int): PortEnableMask = {
    toroidal.requireStandAlone()
    PortEnableMask(
      withOutputPortA = r < toroidal.outerPeRow - 1,
      withOutputPortB = r < toroidal.outerPeRow - 1,
      withInputPortC  = r > 0,
    )
  }

  override def innerVectorPePortMask(
    outerPePortMask: PortEnableMask,
    a: Int,
    b: Int,
  ): PortEnableMask = PortEnableMask(
    withOutputPortA = false,
    withOutputPortB = outerPePortMask.withOutputPortB || a < toroidal.localVectorPeRow - 1,
    withInputPortC  = false,
  )

  //acc
  override def numAccControl: Int = config.outerPeCol
  override def numOuterPeAccControl: Int = 1

  override def needAccEnableC(outerPeIndex: OuterPeIndex): Boolean = false


  override def isVectorPeAccControlSource(outerPeIndex: OuterPeIndex, innerIndex: InnerIndex): Boolean = {
    if(config.needAccumulator) {
      if(config.isStandAloneVectorPeConfig){
        outerPeIndex.r == 0
      } else {
        innerIndex.a == 0 && outerPeIndex.r == 0
      }
    } else {
      false
    }
  }

  override def isStandAloneAccControlCarrier(outerPeIndex: OuterPeIndex): Boolean = {
    if (config.needAccumulator && config.isStandAloneVectorPeConfig) {
      outerPeIndex.r != 0
    } else {
      false
    }
  }

  override def isAccControlSourceIncluded(outerPeIndex: OuterPeIndex): Boolean = {
    if(config.needAccumulator && !config.isStandAloneVectorPeConfig){
      outerPeIndex.r == 0
    } else {
      false
    }

  }

  override def isControlSourceCarrier(outerPeIndex: OuterPeIndex): Boolean = {
    if(config.needAccumulator && !config.isStandAloneVectorPeConfig){
      outerPeIndex.r != 0
    } else {
      false
    }
  }

}

class ToroidalReuseCTopology(toroidal: ToroidalConfig) extends ToroidalTopology(toroidal) {

  //Systolic array dimensions
  override val systolicInputDimA: Int = toroidal.systolicArrayRowDimension
  override val systolicInputDimB: Int = toroidal.systolicArrayColDimension

  override val numAccEnableC: Int = toroidal.totalVectorPeCol

  override val systolicOutputDimC: Int = toroidal.totalVectorPeCol

  //Cluster PE
  override def outerPePortMask(outerPeIndex: OuterPeIndex): PortEnableMask = {
    PortEnableMask(
      withOutputPortA = true,
      withOutputPortB = true,
      withInputPortC  = outerPeIndex.r > 0,
    )
  }

  override val outerPeInputDimA: Int = toroidal.localVectorPeRow * toroidal.multPerPe
  override val outerPeInputDimB: Int = toroidal.localVectorPeCol * toroidal.multPerPe
  override val outerPeDimC: Int = toroidal.localVectorPeCol

  override val numOuterPeAccEnableC: Int = toroidal.localVectorPeCol
  override def needAccEnableC(outerPeIndex: OuterPeIndex): Boolean =
    outerPeIndex.r < toroidal.outerPeRow - 1

  override def outerPeOutputDimA(outerPeIndex: OuterPeIndex): Int = outerPeInputDimA
  override def outerPeOutputDimB(outerPeIndex: OuterPeIndex): Int = outerPeInputDimB

  //Vector PE
  override def standAloneVectorPePortMask(r: Int, c: Int): PortEnableMask = {
    toroidal.requireStandAlone()
    PortEnableMask(
      withOutputPortA = true,
      withOutputPortB = true,
      withInputPortC  = r > 0,
    )
  }

  override def innerVectorPePortMask(
    outerPePortMask: PortEnableMask,
    a: Int,
    b: Int,
  ): PortEnableMask = PortEnableMask(
    withOutputPortA = false,
    withOutputPortB = false,
    // ReuseC drains output-stationary results by chaining inner sub-rows' C ports:
    // sub-row a>0 takes inputC from the sub-row above it; sub-row a==0 takes it from
    // the cluster's inputC, which exists exactly when the cluster is not the top row
    // (outerPePortMask.withInputPortC == outerPeIndex.r > 0). Global row 0 has no
    // upstream C and stays inputC-less.
    withInputPortC  = outerPePortMask.withInputPortC || a > 0,
  )

  override def additionalDelay(outerPeIndex: OuterPeIndex): Int = config.outerPeRow - outerPeIndex.r

  //buffers
  override val wrapBufferCellPort: Int = toroidal.multPerPe

  override def wrapBufferNumCell(tileType: TileType.Value): Int = tileType match {
    // One wrap cell per PHYSICAL row (TypeA) / column (TypeB). With broadcast
    // (lvR/lvC > 1) that is totalVectorPeRow / totalVectorPeCol, not the cluster count.
    case TileType.TypeA => toroidal.totalVectorPeRow
    case TileType.TypeB => toroidal.totalVectorPeCol
    case TileType.TypeC =>
      throw new IllegalArgumentException(
        s"[error] WrapBuffer does not support tileType: $tileType")
  }

  //acc
  override def numAccControl: Int = throw new IllegalArgumentException(
    s"[error] Toroidal ReuseC dose not need accumulator")

  override def numOuterPeAccControl: Int = throw new IllegalArgumentException(
    s"[error] Toroidal ReuseC dose not need accumulator")

  override def isVectorPeAccControlSource(outerPeIndex: OuterPeIndex, innerIndex: InnerIndex): Boolean = {
    false
  }

  override def isStandAloneAccControlCarrier(outerPeIndex: OuterPeIndex): Boolean = {
    false
  }

  override def isAccControlSourceIncluded(outerPeIndex: OuterPeIndex): Boolean = {
    false
  }

  override def isControlSourceCarrier(outerPeIndex: OuterPeIndex): Boolean = {
    false
  }


}