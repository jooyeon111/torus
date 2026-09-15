package torus.topology

import spinal.core.log2Up

import torus.config._
import torus.core._

abstract class GroupedTopology(val grouped: GroupedConfig) extends ArrayTopology {

  override def config: SystolicArrayConfig = grouped

  private def toroidalOnly(name: String): Nothing =
    throw new IllegalArgumentException(s"[error] $name is Toroidal-only")

  // Systolic array dimensions
  override val systolicInputDimA: Int = grouped.systolicArrayRowDimension
  override val systolicInputDimB: Int = grouped.systolicArrayColDimension

  override def numUpdateEnableA: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  override def numUpdateEnableB: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  override def numAccEnableC: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  //Group PE
  override val numVectorPePerGroupPe: Int = grouped.localVectorPeRow * grouped.localVectorPeCol

  override val outerPeInputDimA: Int = grouped.outerPeRowDimension
  override val outerPeInputDimB: Int = grouped.outerPeColDimension

  override def numOuterPeUpdateEnableA: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  override def numOuterPeUpdateEnableB: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  override def numOuterPeAccEnableC: Int =
    throw new IllegalArgumentException(s"[error] This dataflow cannot use this")

  override def outerPeOutputDimA(outerPeIndex: OuterPeIndex): Int = outerPeInputDimA
  override def outerPeOutputDimB(outerPeIndex: OuterPeIndex): Int = outerPeInputDimB

  //Vector PE
  override def additionalDelay(outerPeIndex: OuterPeIndex): Int =
    throw new IllegalArgumentException(s"[error] Grouped ReuseA/B cannot use this")

  //Buffers
  override def skewBufferDepth(tileType: TileType.Value): Int = tileType match {
    case TileType.TypeA => skewBufferStepA
    case TileType.TypeB => skewBufferStepB
    case TileType.TypeC =>
      throw new IllegalArgumentException(
        s"[error] Use postBufferStep(...) for TypeC SkewBuffer")
  }

  override val skewBufferStepA: Int = grouped.outerPeRow
  override val skewBufferStepB: Int = grouped.outerPeCol

  override def skewBufferPortsPerStep(tileType: TileType.Value): Int = tileType match {
    case TileType.TypeA => skewBufferPortsPerStepA
    case TileType.TypeB => skewBufferPortsPerStepB
    case TileType.TypeC =>
      throw new IllegalArgumentException(
        s"[error] TypeC SkewBuffer has no portsPerDelay (defaults to 1)")
  }

  override val skewBufferPortsPerStepA: Int = grouped.localVectorPeRow * grouped.multPerPe
  override val skewBufferPortsPerStepB: Int = grouped.localVectorPeCol * grouped.multPerPe

  override def wrapBufferCellPort: Int =
    throw new IllegalArgumentException("[error] WrapBuffer is Toroidal ReuseC only")

  override def wrapBufferNumCell(tileType: TileType.Value): Int =
    throw new IllegalArgumentException("[error] WrapBuffer is Toroidal ReuseC only")

  //acc
  override def numAccControl: Int = 1

  override def numOuterPeAccControl: Int = {
    throw new IllegalArgumentException(
      s"[error] Use postBufferStep(...) for TypeC SkewBuffer")
  }

  override def needVectorPeAccControlPort(outerPeIndex: OuterPeIndex, innerIndex: InnerIndex): Boolean =
    false

  override def isVectorPeAccControlSource(outerPeIndex: OuterPeIndex, innerIndex: InnerIndex): Boolean =
    toroidalOnly("isVectorPeAccControlSource")

  override def isStandAloneAccControlCarrier(outerPeIndex: OuterPeIndex): Boolean =
    toroidalOnly("isStandAloneAccControlCarrier")

  override def needClusterPeAccControlPort(outerPeIndex: OuterPeIndex): Boolean =
    false

  override def isAccControlSourceIncluded(outerPeIndex: OuterPeIndex): Boolean =
    toroidalOnly("isAccControlSourceIncluded")

  override def isControlSourceCarrier(outerPeIndex: OuterPeIndex): Boolean =
    toroidalOnly("isControlSourceCarrier")

}

class GroupedReuseATopology(grouped: GroupedConfig) extends GroupedTopology(grouped) {

  //Systolic array
  override val systolicOutputDimC: Int = grouped.totalVectorPeRow
  override val numUpdateEnableA: Int = grouped.totalVectorPeRow

  //Group PE
  override def outerPePortMask(outerPeIndex: OuterPeIndex): PortEnableMask = {
    val r = outerPeIndex.r
    val c = outerPeIndex.c
    PortEnableMask(
      withOutputPortA = c < grouped.outerPeCol - 1,
      withOutputPortB = r < grouped.outerPeRow - 1,
      withInputPortC  = c > 0,
    )
  }

  override val outerPeDimC: Int = grouped.localVectorPeRow
  override val numOuterPeUpdateEnableA: Int = grouped.localVectorPeRow
  override def needAccEnableC(outerPeIndex: OuterPeIndex): Boolean = false

  //Vector PE
  override def standAloneVectorPePortMask(r: Int, c: Int): PortEnableMask = {
    grouped.requireStandAlone()
    PortEnableMask(
      withOutputPortA = c < grouped.outerPeCol - 1,
      withOutputPortB = r < grouped.outerPeRow - 1,
      withInputPortC  = c > 0,
    )
  }

  override def innerVectorPePortMask(
    outerPePortMask: PortEnableMask,
    a: Int,
    b: Int,
  ): PortEnableMask = PortEnableMask(
    withOutputPortA = outerPePortMask.withOutputPortA || b < grouped.localVectorPeCol - 1,
    withOutputPortB = false,
    withInputPortC  = false,
  )

  //Buffers
  override def postBufferStep: Int = grouped.outerPeRow
  override def postBufferPortsPerStep: Int = grouped.localVectorPeRow

  //acc
  override val numAccTapCellPort: Int = config.localVectorPeRow
  override def accTapPreludeDelay: Int = skewBufferStepB
  override def accTapCellDepth: Int =
    (1 + log2MultPerPe) + log2Up(grouped.localVectorPeCol) + 1

}

class GroupedReuseBTopology(grouped: GroupedConfig) extends GroupedTopology(grouped) {

  //Systolic array
  override val systolicOutputDimC: Int = grouped.totalVectorPeCol
  override val numUpdateEnableB: Int = grouped.totalVectorPeCol

  //Group PE
  override def outerPePortMask(outerPeIndex: OuterPeIndex): PortEnableMask = {
    val r = outerPeIndex.r
    val c = outerPeIndex.c
    PortEnableMask(
      withOutputPortA = c < grouped.outerPeCol - 1,
      withOutputPortB = r < grouped.outerPeRow - 1,
      withInputPortC  = r > 0,
    )
  }

  override val outerPeDimC: Int = grouped.localVectorPeCol
  override val numOuterPeUpdateEnableB: Int = grouped.localVectorPeCol
  override def needAccEnableC(outerPeIndex: OuterPeIndex): Boolean = false

  //Vector PE
  override def standAloneVectorPePortMask(r: Int, c: Int): PortEnableMask = {
    grouped.requireStandAlone()
    PortEnableMask(
      withOutputPortA = c < grouped.outerPeCol - 1,
      withOutputPortB = r < grouped.outerPeRow - 1,
      withInputPortC  = r > 0,
    )
  }

  override def innerVectorPePortMask(
    outerPePortMask: PortEnableMask,
    a: Int,
    b: Int,
  ): PortEnableMask = PortEnableMask(
    withOutputPortA = false,
    withOutputPortB = outerPePortMask.withOutputPortB || a < grouped.localVectorPeRow - 1,
    withInputPortC  = false,
  )

  //Buffers
  override def postBufferStep: Int = grouped.outerPeCol
  override def postBufferPortsPerStep: Int = grouped.localVectorPeCol

  // VectorPE multiplier tree + GroupPE adder tree + output register
  override val numAccTapCellPort: Int = config.localVectorPeCol
  override def accTapPreludeDelay: Int = skewBufferStepA
  override def accTapCellDepth: Int =
    (1 + log2MultPerPe) + log2Up(grouped.localVectorPeRow) + 1

}

class GroupedReuseCTopology(grouped: GroupedConfig) extends GroupedTopology(grouped) {

  //systolic array
  override val systolicOutputDimC: Int = grouped.outerPeCol * numVectorPePerGroupPe
  override val numAccEnableC: Int = grouped.totalVectorPeCol

  //Group PE
  override def outerPePortMask(outerPeIndex: OuterPeIndex): PortEnableMask = {
    val r = outerPeIndex.r
    val c = outerPeIndex.c
    PortEnableMask(
      withOutputPortA = c < grouped.outerPeCol - 1,
      withOutputPortB = r < grouped.outerPeRow - 1,
      withInputPortC = r > 0
    )
  }

  override val outerPeDimC: Int = numVectorPePerGroupPe
  override val numOuterPeAccEnableC: Int = grouped.localVectorPeCol
  override def needAccEnableC(outerPeIndex: OuterPeIndex): Boolean =
    outerPeIndex.r < grouped.outerPeRow - 1

  //Vector PE
  override def standAloneVectorPePortMask(r: Int, c: Int): PortEnableMask = {
    grouped.requireStandAlone()
    PortEnableMask(
      withOutputPortA = c < grouped.outerPeCol - 1,
      withOutputPortB = r < grouped.outerPeRow - 1,
      //      withInputPortC  = r > 0 && c < grouped.outerPeCol - 1,
      withInputPortC = r > 0
    )
  }

  override def innerVectorPePortMask(
    outerPePortMask: PortEnableMask,
    a: Int,
    b: Int,
  ): PortEnableMask = PortEnableMask(
    withOutputPortA = false,
    withOutputPortB = false,
    withInputPortC  = false,
  )

  override def additionalDelay(outerPeIndex: OuterPeIndex): Int = 0

  //Buffers
  override def postBufferStep: Int = grouped.outerPeCol
  override def postBufferPortsPerStep: Int = numVectorPePerGroupPe

  //acc
  override def numAccTapCellPort: Int =
    throw new IllegalArgumentException(s"[error] Grouped ReuseC dose not use numAccTapCellPort")

  override def accTapPreludeDelay: Int =
    throw new IllegalArgumentException(s"[error] Grouped ReuseC dose not use numAccTapCellPort")

  override def accTapCellDepth: Int =
    throw new IllegalArgumentException(s"[error] Grouped ReuseC dose not use accTapCellDepth")

}
