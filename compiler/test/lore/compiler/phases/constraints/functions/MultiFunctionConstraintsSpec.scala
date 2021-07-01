package lore.compiler.phases.constraints.functions

import lore.compiler.phases.constraints.MultiFunctionConstraints
import lore.compiler.test.BaseSpec

class MultiFunctionConstraintsSpec extends BaseSpec {
  private val fragmentBase = "phases/constraints/functions"

  "constraints/functions/output-types" should "be compiled with 'incompatible output types' errors" in {
    assertCompilationErrors(s"$fragmentBase/incompatible-output-types.lore") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 8),
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 11),
      ))
    }
  }

  "constraints/functions/not-fully-implemented" should "be compiled with 'function is not fully implemented' errors" in {
    assertCompilationErrors(s"$fragmentBase/not-fully-implemented.lore") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 12),
        ErrorSignature(classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 13),
        ErrorSignature(classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 15),
        ErrorSignature(classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 18),
        ErrorSignature(classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 19),
      ))
    }
  }
}
