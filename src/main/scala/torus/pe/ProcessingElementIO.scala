package torus.pe

import spinal.core._

import torus.accumulator._

/**
 * The port vocabulary every PE speaks, at both levels of the hierarchy.
 *
 * A structural type, not a base class: it says only that `io` is a Bundle carrying these
 * fields, so array code can wire a [[VectorPE]] and an [[OuterPE]] through the same loop
 * without knowing which it holds. That is what lets a fabric drop the cluster level
 * entirely when `localVectorPeRow == localVectorPeCol == 1` and hang VectorPEs straight
 * off the array — both satisfy this type.
 *
 * Read the fields as PAIRS. `inputX` / `outputX` are the two ends of a PE's pass-through
 * for tile X, and the enables travel alongside the data they gate rather than being
 * broadcast: `inputUpdateEnableA` follows A down the same chain so the capture pulse
 * stays aligned with the operand at every hop.
 *
 * IMPORTANT — most of these fields are NULL on most PEs. Each is built with SpinalHDL's
 * `generate`, which yields null when its condition is false, and the conditions are the
 * dataflow and the PE's position: `inputUpdateEnableA` exists only for ReuseA,
 * `outputA` only where [[torus.core.PortEnableMask]] kept the port, the dual-mode lanes
 * only on a dual-mode Toroidal array. Declaring them all unconditionally here is what
 * keeps the type one shape; the safety is that every reader guards with the SAME
 * predicate that created the port. Touching one unguarded is a null dereference at
 * elaboration, not a width error — the usual symptom of a guard that drifted from its
 * port.
 *
 * `Vec` everywhere, even where the width is 1: a VectorPE carries one lane and a cluster
 * carries many, and a single indexed form spares every wiring loop a special case.
 */
trait ProcessingElementIO[InputType <: Data, AccType <: Data] {

  val io: Bundle {
    val inputA: Vec[InputType]
    val inputB: Vec[InputType]
    val inputC: Vec[AccType]

    // Toroidal DualMode only (null/absent otherwise): dedicated dual-mode input lane(s)
    // on the STREAMED operand plus the shared MAC-input select. ReuseB streams A
    // (inputA2/selectA), ReuseA mirrors it and streams B (inputB2/selectB); only one
    // pair exists at a time. See VectorPE / ToroidalSystolicArray.
    val inputA2: Vec[InputType]
    val selectA: Bool
    val inputB2: Vec[InputType]
    val selectB: Bool

    val inputUpdateEnableA: Vec[Bool]
    val inputUpdateEnableB: Vec[Bool]
    val inputAccEnableC: Vec[Bool]
    val accControlInput : Vec[AccControl]

    val outputA: Vec[InputType]
    val outputB: Vec[InputType]
    val outputC: Vec[AccType]

    // Toroidal DualMode only (null/absent otherwise): the locally reduced dot
    // product(s) of the PE's VectorPE(s), raw (pre-resize), tapped for the
    // array-level DualModeAdderTree. One element per inner VectorPE, VectorPE-major.
    val outputPartial: Vec[AccType]

    val outputUpdateEnableA: Vec[Bool]
    val outputUpdateEnableB: Vec[Bool]
    val outputAccEnableC: Vec[Bool]
    val accControlOutput: Vec[AccControl]

  }

}
