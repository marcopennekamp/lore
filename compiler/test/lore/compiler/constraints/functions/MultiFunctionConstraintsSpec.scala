package lore.compiler.constraints.functions

import lore.compiler.constraints.MultiFunctionConstraints
import lore.compiler.test.BaseSpec

class MultiFunctionConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints/functions"

  "constraints/functions/illegally-abstract" should "be compiled with 'function illegally abstract' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/illegally-abstract.lore")(
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

  "constraints/functions/output-types" should "be compiled with 'incompatible output types' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/incompatible-output-types.lore")(
      (classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 8),
      (classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 11),
    )
  }

  "constraints/functions/not-fully-implemented" should "be compiled with 'function is not fully implemented' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/not-fully-implemented.lore")(
      (classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 12),
      (classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 13),
      (classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 15),
      (classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 18),
      (classOf[MultiFunctionConstraints.AbstractFunctionNotImplemented], 19),
    )
  }
}
