package torus.topology

import spinal.core.log2Up

import torus.config._
import torus.core._

/**
 * Topology of a systolic array.
 *
 *   ArrayTopology (trait)
 *    ├── GroupedTopology (abstract)
 *    │    ├── GroupedReuseATopology
 *    │    ├── GroupedReuseBTopology
 *    │    └── GroupedReuseCTopology
 *    └── ToroidalTopology (abstract)
 *         ├── ToroidalReuseATopology
 *         ├── ToroidalReuseBTopology
 *         └── ToroidalReuseCTopology
 *
 **/

trait ArrayTopology {

  def config: SystolicArrayConfig

  // Systolic array dimensions
  def systolicInputDimA: Int
  def systolicInputDimB: Int
  def systolicOutputDimC: Int

  def numUpdateEnableA: Int
  def numUpdateEnableB: Int
  def numAccEnableC: Int
  def numAccControl: Int

  //Outer PE Dimensions ( Group PE (Grouped) + Cluster PE (Toroidal))

  def numVectorPePerGroupPe: Int
  def outerPePortMask(outerPeIndex: OuterPeIndex): PortEnableMask

  def outerPeInputDimA: Int
  def outerPeInputDimB: Int
  def outerPeDimC: Int

  def numOuterPeUpdateEnableA: Int
  def numOuterPeUpdateEnableB: Int
  def numOuterPeAccEnableC: Int

  def outerPeOutputDimA(outerPeIndex: OuterPeIndex): Int
  def outerPeOutputDimB(outerPeIndex: OuterPeIndex): Int

  def needAccEnableC(outerPeIndex: OuterPeIndex): Boolean
  def numOuterPeAccControl: Int
  def log2LocalVectorPeRow: Int = log2Up(config.localVectorPeRow)
  def log2LocalVectorPeCol: Int = log2Up(config.localVectorPeCol)
  def log2MultPerPe: Int = log2Up(config.multPerPe)

  //Vector PE dimensions

  def standAloneVectorPePortMask(r: Int, c: Int): PortEnableMask
  def innerVectorPePortMask(outerPePortMask: PortEnableMask, a: Int, b: Int): PortEnableMask

  //TODO change additionalDelay name
  def additionalDelay(outerPeIndex: OuterPeIndex): Int

  def isVectorPeAccControlSource(outerPeIndex: OuterPeIndex, innerIndex: InnerIndex): Boolean
  def isStandAloneAccControlCarrier(outerPeIndex: OuterPeIndex): Boolean
  def needVectorPeAccControlPort(outerPeIndex: OuterPeIndex, innerIndex: InnerIndex): Boolean
  def needClusterPeAccControlPort(outerPeIndex: OuterPeIndex): Boolean
  def isAccControlSourceIncluded(outerPeIndex: OuterPeIndex): Boolean
  def isControlSourceCarrier(outerPeIndex: OuterPeIndex): Boolean

  //Buffers
  def syncBufferNumPort(tileType: TileType.Value): Int = tileType match {
    case TileType.TypeA => systolicInputDimA
    case TileType.TypeB => systolicInputDimB
    case TileType.TypeC =>
      throw new IllegalArgumentException(
        s"[error] SyncBuffer does not support tileType: $tileType")
  }

  def skewBufferDepth(tileType: TileType.Value): Int
  def skewBufferStepA: Int
  def skewBufferStepB: Int

  def skewBufferPortsPerStep(tileType: TileType.Value): Int
  def skewBufferPortsPerStepA: Int
  def skewBufferPortsPerStepB: Int

  def wrapBufferCellPort: Int
  def wrapBufferNumCell(tileType: TileType.Value): Int

  def postBufferStep: Int
  def postBufferPortsPerStep: Int

  //accumulator
  def numAccTapCellPort: Int
  def accTapCellDepth: Int
  def accTapPreludeDelay: Int

}

object ArrayTopology {
  def apply(config: SystolicArrayConfig): ArrayTopology = config match {
    case grouped: GroupedConfig => grouped.dataflow match {
      case Dataflow.ReuseA => new GroupedReuseATopology(grouped)
      case Dataflow.ReuseB => new GroupedReuseBTopology(grouped)
      case Dataflow.ReuseC => new GroupedReuseCTopology(grouped)
    }
    case toroidal: ToroidalConfig => toroidal.dataflow match {
      case Dataflow.ReuseA => new ToroidalReuseATopology(toroidal)
      case Dataflow.ReuseB => new ToroidalReuseBTopology(toroidal)
      case Dataflow.ReuseC => new ToroidalReuseCTopology(toroidal)
    }
    case _: ToroidalDualModeConfig =>
      throw new IllegalArgumentException(
        "[error] ToroidalDualModeConfig has no topology of its own: dual mode adds ports " +
          "and entry stages AROUND a stock Toroidal fabric. Ask cfg.toToroidalConfig for it.")
  }
}
