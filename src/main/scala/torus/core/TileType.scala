package torus.core

/**
 * Which of the three GEMM tiles a buffer or topology query is about: `TypeA` and `TypeB`
 * the operands of C = A·B, `TypeC` the result.
 *
 * Used to size the datapath's buffers from one set of topology methods instead of three
 * parallel ones — `skewBufferDepth`, `skewBufferPortsPerStep`, `wrapBufferNumCell`,
 * `syncBufferNumPort` all take a `TileType` and answer per tile.
 *
 */
object TileType extends Enumeration {
  val TypeA, TypeB, TypeC = Value
}
