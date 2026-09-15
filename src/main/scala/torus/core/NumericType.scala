package torus.core

/**
 * Signedness of every port in an elaborated array — the config field that decides which
 * SpinalHDL hardware type the whole fabric is built out of.
 *
 * This is not a per-port choice. It is read exactly once, in `GenerationMain`, to pick
 * the `Arithmetic` and `PortTypeProvider` instances for the generic parameters
 * `[InputType, AccType]`, and from there the array elaborates as either all-`SInt` or
 * all-`UInt`. Mixed signedness is not representable, by construction.
 *
 * `name` carries the SpinalHDL type's own spelling because it is the string that reaches
 * generated Verilog: `SystolicArray` prefixes the top module's name with it, so
 * `SInt_ReuseB_Toroidal_SystolicArray_...` identifies the netlist. Renaming these values
 * renames every generated module.
 *
 * Sealed with two case objects rather than an `Enumeration` (unlike its neighbours here)
 * so that the `GenerationMain` match is checked for exhaustiveness — the one place where
 * a missed case would mean an array that silently cannot be built.
 */
sealed trait NumericType {
  def name: String
}

object NumericType {
  case object Signed extends NumericType {
    val name = "SInt"
  }
  case object Unsigned extends NumericType {
    val name = "UInt"
  }
}
