package lore.compiler.constraints.variables

import lore.compiler.feedback.{GlobalVariableFeedback, MultiFunctionFeedback}
import lore.compiler.test.BaseSpec

class GlobalVariableConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints/variables"

  "constraints/variables/duplicates" should "be compiled with various 'duplicate name' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/duplicates.lore")(
      (classOf[GlobalVariableFeedback.AlreadyExists], 2),
      (classOf[GlobalVariableFeedback.AlreadyExists], 4),
      (classOf[MultiFunctionFeedback.NameTakenByVariable], 6),
    )
  }
}
