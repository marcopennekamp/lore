package lore.compiler.constraints.functions

import lore.compiler.constraints.MultiFunctionConstraints
import lore.compiler.feedback.MultiFunctionFeedback
import lore.compiler.test.BaseSpec

class MultiFunctionConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints/functions"

  s"$fragmentBase/illegally_abstract" should "be compiled with 'function illegally abstract' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/illegally_abstract.lore")(
      (classOf[MultiFunctionFeedback.Abstract.IllegallyAbstract], 7),
      (classOf[MultiFunctionFeedback.Abstract.IllegallyAbstract], 9),
      (classOf[MultiFunctionFeedback.Abstract.IllegallyAbstract], 11),
      (classOf[MultiFunctionFeedback.Abstract.IllegallyAbstract], 13),
      (classOf[MultiFunctionFeedback.Abstract.IllegallyAbstract], 15),
      (classOf[MultiFunctionFeedback.Abstract.IllegallyAbstract], 17),
      (classOf[MultiFunctionFeedback.Abstract.IllegallyAbstract], 19),
      (classOf[MultiFunctionFeedback.Abstract.IllegallyAbstract], 21),
      (classOf[MultiFunctionFeedback.Abstract.IllegallyAbstract], 23),
      (classOf[MultiFunctionFeedback.Abstract.IllegallyAbstract], 25),
    )
  }

  s"$fragmentBase/incompatible_output_types" should "be compiled with 'incompatible output types' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/incompatible_output_types.lore")(
      (classOf[MultiFunctionFeedback.IncompatibleOutputTypes], 8),
      (classOf[MultiFunctionFeedback.IncompatibleOutputTypes], 11),
    )
  }

  s"$fragmentBase/missing_type_parameters" should "be compiled with 'missing type parameters' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/missing_type_parameters.lore")(
      (classOf[MultiFunctionFeedback.TypeParametersMissing], 2),
    )
  }

  s"$fragmentBase/not_fully_implemented" should "be compiled with 'function is not fully implemented' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/not_fully_implemented.lore")(
      (classOf[MultiFunctionFeedback.Abstract.NotTotal], 12),
      (classOf[MultiFunctionFeedback.Abstract.NotTotal], 13),
      (classOf[MultiFunctionFeedback.Abstract.NotTotal], 15),
      (classOf[MultiFunctionFeedback.Abstract.NotTotal], 18),
      (classOf[MultiFunctionFeedback.Abstract.NotTotal], 19),
    )
  }
}
