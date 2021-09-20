package lore.compiler.resolution

import lore.compiler.test.BaseSpec

class ImportSpec extends BaseSpec {

  private val fragmentBase = "resolution"

  "resolution/imports" should "be compiled with various import errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/imports.lore")(
      (classOf[TypeDependencies.InheritanceCycle], 1),
      (classOf[TypeDependencies.InheritanceCycle], 1),
      (classOf[TypeDependencies.InheritanceCycle], 2),
      (classOf[TypeDependencies.InheritanceCycle], 2),
      (classOf[TypeDependencies.InheritanceCycle], 3),
      (classOf[Scope.UnknownEntry], 4),
      (classOf[TypeDependencies.InheritanceCycle], 9),
      (classOf[TypeDependencies.InheritanceCycle], 10),
      (classOf[TypeDependencies.InheritanceCycle], 13),
      (classOf[TypeDependencies.InheritanceCycle], 15),
      (classOf[TypeDependencies.InheritanceCycle], 16),
    )
  }

}
