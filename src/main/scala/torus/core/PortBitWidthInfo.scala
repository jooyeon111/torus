package torus.core

/**
 * The three datapath widths of one PE's ports, bundled with their positivity checks.
 *
 * NOTE: nothing constructs this. Port widths in the live code are computed per position
 * by [[torus.arithmetic.PortTypeProvider]] — `createVectorPeInputTypeC` and friends
 * widen the C port with depth down the accumulation chain — which is precisely what a
 * flat record of three fixed widths cannot express. Treat this as superseded rather than
 * as the place to add a width.
 *
 * `bitWidthSystolicOutputC` is optional because a PE only carries a C port where the
 * dataflow and its position give it one; the `foreach` is the idiom for "check it if it
 * is there", since `require` on an absent width would reject the legitimate case.
 */
case class PortBitWidthInfo(
  bitWidthInputA: Int,
  bitWidthInputB: Int,
  bitWidthSystolicOutputC: Option[Int] = None,
) {

  require(bitWidthInputA > 0, "Bit width for Input A must be positive")
  require(bitWidthInputB > 0, "Bit width for Input B must be positive")

  bitWidthSystolicOutputC.foreach { width =>
    require(width > 0, "Bit width for Systolic Output C must be positive")
  }

}
