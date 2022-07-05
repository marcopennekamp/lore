package lore.compiler.resolution.variables

import lore.compiler.feedback.{ModuleFeedback, MultiFunctionFeedback}
import lore.compiler.test.BaseSpec

class GlobalVariableResolutionSpec extends BaseSpec {
  private val fragmentBase = "resolution/variables"

  s"$fragmentBase/duplicates" should "be compiled with various 'duplicate name' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/duplicates.lore")(
      (classOf[ModuleFeedback.MemberNameTaken], 4),
      (classOf[ModuleFeedback.MemberNameTaken], 6),
    )
  }
}
