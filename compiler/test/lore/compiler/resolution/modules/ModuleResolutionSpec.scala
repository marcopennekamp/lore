package lore.compiler.resolution.modules

import lore.compiler.feedback.{ModuleFeedback, MultiFunctionFeedback}
import lore.compiler.test.BaseSpec

class ModuleResolutionSpec extends BaseSpec {

  private val fragmentBase = "resolution/modules"

  s"$fragmentBase/duplicates" should "be compiled with various 'name taken' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/duplicates.lore")(
      (classOf[GlobalVariableFeedback.NameTaken], 2),
      (classOf[MultiFunctionFeedback.NameTaken], 5),
      (classOf[ModuleFeedback.MemberNameTaken], 8),
    )
  }

  s"$fragmentBase/imports" should "be compiled with various import errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/imports.lore")(
      (classOf[ModuleFeedback.Import.TooShort], 2),
      (classOf[ModuleFeedback.Import.UnresolvedHeadSegment], 5),
      (classOf[ModuleFeedback.Import.NotFound], 8),
      (classOf[ModuleFeedback.Import.Wildcard.NotFound], 11),
    )
  }

}
