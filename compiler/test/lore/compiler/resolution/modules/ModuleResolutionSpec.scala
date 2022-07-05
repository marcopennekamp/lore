package lore.compiler.resolution.modules

import lore.compiler.feedback.ModuleFeedback
import lore.compiler.test.BaseSpec

class ModuleResolutionSpec extends BaseSpec {

  // TODO (multi-import): Test various new import errors.

  private val fragmentBase = "resolution/modules"

  s"$fragmentBase/duplicates" should "be compiled with various 'name taken' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/duplicates.lore")(
      (classOf[ModuleFeedback.MemberNameTaken], 5),
      (classOf[ModuleFeedback.MemberNameTaken], 8),
    )
  }

  s"$fragmentBase/imports" should "be compiled with various import errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/imports.lore")(
      (classOf[ModuleFeedback.Import.TooShort], 2),
      (classOf[ModuleFeedback.Import.NotFound], 5),
      (classOf[ModuleFeedback.Import.NotFound], 8),
      (classOf[ModuleFeedback.Import.NotFound], 11),
      (classOf[ModuleFeedback.Import.ModuleExpected], 14),
    )
  }

}
