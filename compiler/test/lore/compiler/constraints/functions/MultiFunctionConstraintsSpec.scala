package lore.compiler.constraints.functions

import lore.compiler.constraints.MultiFunctionConstraints
import lore.compiler.feedback.MultiFunctionFeedback
import lore.compiler.test.BaseSpec

class MultiFunctionConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints/functions"

  s"$fragmentBase/illegally-abstract" should "be compiled with 'function illegally abstract' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/illegally_abstract.lore")(
      (classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 7),
      (classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 9),
      (classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 11),
      (classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 13),
      (classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 15),
      (classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 17),
      (classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 19),
      (classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 21),
      (classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 23),
      (classOf[MultiFunctionConstraints.FunctionIllegallyAbstract], 25),
    )
  }

  s"$fragmentBase/incompatible-output-types" should "be compiled with 'incompatible output types' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/incompatible_output_types.lore")(
      (classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 8),
      (classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 11),
    )
  }

  s"$fragmentBase/missing-type-parameters" should "be compiled with 'missing type parameters' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/missing_type_parameters.lore")(
      (classOf[MultiFunctionFeedback.TypeParametersMissing], 2),
    )
  }

  s"$fragmentBase/not-fully-implemented" should "be compiled with 'function is not fully implemented' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/not_fully_implemented.lore")(
      (classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 12),
      (classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 13),
      (classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 15),
      (classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 18),
      (classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 19),
    )
  }
}
