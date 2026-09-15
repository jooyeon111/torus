package torus.buffer

import spinal.core._

/**
 * The one shape every operand-entry buffer presents: a `Vec` in, a same-sized `Vec`
 * out, and nothing else.
 *
 * It exists so an array can hold "the preprocessor for operand A" as a single type
 * without caring WHICH buffer the dataflow called for. The three implementations are
 * interchangeable at that slot and differ only in what they do between the two Vecs:
 * {{{
 *   RegisterBuffer   one flat cycle on every port          (stationary operand)
 *   SkewBuffer       a per-port delay ramp, 1..depth       (streamed operand, Grouped)
 *   WrapBuffer       a cycle, plus a stream/recirculate mux (Toroidal ReuseC)
 * }}}
 * Hence `GroupedSystolicArray.createPreprocessorA` and its Toroidal twin can return
 * `Component with SkewBufferIO[InputType]` and let `wireA` stay dataflow-agnostic.
 *
 * Declared as a trait over a structural `io` type rather than a shared base Component
 * because the implementations build their bundles differently — [[WrapBuffer]]'s has
 * extra ports (`rotatedInput`, `selInput`) that the other two must not carry.
 */
trait SkewBufferIO[T <: Data] {

  val io: Bundle {
    val input: Vec[T]
    val output: Vec[T]
  }
}
