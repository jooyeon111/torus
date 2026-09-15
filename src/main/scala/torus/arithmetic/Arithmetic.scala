package torus.arithmetic

import spinal.core._
import scala.language.implicitConversions

/**
 * The arithmetic every datapath in the design is written against.
 *
 * The whole fabric (PEs, adder trees, accumulator) is generic in two type parameters:
 * `InputType` — the operand type on the A / B buses — and `AccType` — the type every
 * product and partial sum lives in. This trait is what makes that generic code
 * elaborable: it supplies the three things a datapath actually needs from a number
 * system, and nothing else.
 *
 *  - a multiply `InputType * InputType => AccType`   (via [[InputArithmeticOps]])
 *  - add / widening-add / resize on `AccType`        (via [[AccArithmeticOps]])
 *  - typed zeros, for `Reg` inits and `Delay` inits
 *
 * Only the OPERATIONS live here. The WIDTHS are chosen elsewhere, by
 * [[PortTypeProvider]], which is why every method here takes an explicit `width`
 * rather than deriving one: this trait must not know how far a partial sum has grown
 * along a chain.
 *
 * Usage is implicit throughout — a component takes `implicit arithmetic:
 * Arithmetic[InputType, AccType]`, and `castInput` / `castAcc` then make plain `a * b`
 * and `x +^ y` resolve on the generic types. Pick the instance with the numeric type:
 * `Arithmetic.sIntArithmetic` for a Signed config, `uIntArithmetic` for an Unsigned one
 * (matching the [[SignedPortTypeProvider]] / [[UnsignedPortTypeProvider]] pair).
 *
 * Adding a number system (fixed point, bfloat, …) means adding one instance here plus
 * its `PortTypeProvider`; no component changes.
 */
trait Arithmetic[InputType <: Data, AccType <: Data] {

  // Implicit CONVERSIONS, not plain methods: bringing an `Arithmetic` into scope is
  // what puts `*`, `+`, `+^` and `resizeTo` on the abstract `InputType` / `AccType`
  // values, which as bare `Data` have no such operators.
  implicit def castInput(t: InputType): InputArithmeticOps[InputType, AccType]
  implicit def castAcc(t: AccType): AccArithmeticOps[AccType]

  // Width comes from the caller ([[PortTypeProvider]]), never from here — the same
  // zero is needed at a dozen different widths along a growing accumulation chain.
  def zeroInput(width: Int): InputType
  def zeroAccumulation(width: Int): AccType

}

/** Operand-side operations: the one place `InputType` crosses over into `AccType`. */
abstract class InputArithmeticOps[InputType <: Data, AccType <: Data](self: InputType) {
  /** Full-precision product — the result is `bitWidthA + bitWidthB` wide, never truncated. */
  def *(other: InputType): AccType
}

/** Accumulation-side operations, all `AccType -> AccType`. */
abstract class AccArithmeticOps[AccType <: Data](self: AccType) {
  /** Same-width add: the sum stays at `self`'s width and may wrap. */
  def +(other: AccType): AccType

  /**
   * WIDENING add (`+^`): the result is one bit wider than the operands. Adder trees and
   * accumulator chains use this one, because the width they were provisioned with
   * already counts that growth — see [[PortTypeProvider]], whose per-stage widths are
   * exactly `multOutputBitWidth + log2Up(number of products summed so far)`. Using `+`
   * there would silently truncate the carry those widths were sized for.
   */
  def +^(other: AccType): AccType

  /**
   * Resize to match ANOTHER value's width rather than a literal width. Chain stages are
   * declared by `PortTypeProvider` as hardware types, so the target width is normally
   * only available as a type instance; taking the value avoids every call site having
   * to unpack `.getWidth` itself.
   */
  def resizeTo(t: AccType): AccType
}

object Arithmetic {

  // The two instances below are the only number systems the generator supports today,
  // and they are IMPLICIT VALS so `NumericType.Signed` / `Unsigned` in the config picks
  // one up without any component naming it.
  //
  // Both fix `AccType == InputType`. That is not a simplification: SpinalHDL's `SInt *
  // SInt` already returns a full-width `SInt`, so a separate accumulation type would
  // only add casts. A number system where the product type genuinely differs (fixed
  // point with a different Q format, say) is exactly what the two type parameters exist
  // for.

  // ============ SInt ============
  implicit val sIntArithmetic: Arithmetic[SInt, SInt] = new Arithmetic[SInt, SInt] {

    override implicit def castInput(self: SInt): InputArithmeticOps[SInt, SInt] =
      new InputArithmeticOps[SInt, SInt](self) {
        override def *(other: SInt): SInt = self * other
      }

    override implicit def castAcc(self: SInt): AccArithmeticOps[SInt] =
      new AccArithmeticOps[SInt](self) {
        override def +(other: SInt): SInt = self + other
        override def +^(other: SInt): SInt = self +^ other
        // `SInt.resize` is SIGN-extending, which is what keeps a narrow negative partial
        // sum correct when it is widened into a later chain stage.
        override def resizeTo(t: SInt): SInt = self.resize(t.getWidth)
      }

    override def zeroInput(width: Int): SInt = S(0, width bits)
    override def zeroAccumulation(width: Int): SInt = S(0, width bits)
  }

  // ============ UInt ============
  implicit val uIntArithmetic: Arithmetic[UInt, UInt] = new Arithmetic[UInt, UInt] {

    override implicit def castInput(self: UInt): InputArithmeticOps[UInt, UInt] =
      new InputArithmeticOps[UInt, UInt](self) {
        override def *(other: UInt): UInt = self * other
      }

    override implicit def castAcc(self: UInt): AccArithmeticOps[UInt] =
      new AccArithmeticOps[UInt](self) {
        override def +(other: UInt): UInt = self + other
        override def +^(other: UInt): UInt = self +^ other
        // Zero-extending here, the unsigned counterpart of the SInt sign extension.
        override def resizeTo(t: UInt): UInt = self.resize(t.getWidth)
      }

    override def zeroInput(width: Int): UInt = U(0, width bits)
    override def zeroAccumulation(width: Int): UInt = U(0, width bits)
  }

}
