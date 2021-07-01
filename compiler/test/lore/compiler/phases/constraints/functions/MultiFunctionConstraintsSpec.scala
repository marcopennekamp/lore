package lore.compiler.phases.constraints.functions

import lore.compiler.phases.constraints.MultiFunctionConstraints
import lore.compiler.test.BaseSpec

class MultiFunctionConstraintsSpec extends BaseSpec {
  private val fragmentBase = "phases/constraints/functions"

  "constraints/functions/illegally-abstract" should "be compiled with 'function illegally abstract' errors" in {
    assertCompilationErrors(s"$fragmentBase/illegally-abstract.lore") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 7),
        ErrorSignature(classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 9),
        ErrorSignature(classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 11),
        ErrorSignature(classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 13),
        ErrorSignature(classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 15),
        ErrorSignature(classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 17),
        ErrorSignature(classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 19),
        ErrorSignature(classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 21),
        ErrorSignature(classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 23),
        ErrorSignature(classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 25),
      ))
    }
  }

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
