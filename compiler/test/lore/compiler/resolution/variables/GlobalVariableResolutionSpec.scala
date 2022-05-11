package lore.compiler.resolution.variables

import lore.compiler.feedback.{GlobalVariableFeedback, MultiFunctionFeedback}
import lore.compiler.test.BaseSpec

class GlobalVariableResolutionSpec extends BaseSpec {
  private val fragmentBase = "resolution/variables"

  s"$fragmentBase/duplicates" should "be compiled with various 'duplicate name' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/duplicates.lore")(
      (classOf[GlobalVariableFeedback.AlreadyExists], 2),
      (classOf[GlobalVariableFeedback.NameTaken], 2),
      (classOf[GlobalVariableFeedback.AlreadyExists], 4),
      (classOf[MultiFunctionFeedback.NameTaken], 6),
    )
  }
}
