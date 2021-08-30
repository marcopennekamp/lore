package lore.compiler.resolution

import lore.compiler.semantics.scopes.Scope
import lore.compiler.test.BaseSpec

class TypeDependencySpec extends BaseSpec {

  private val fragmentBase = "resolution"

  "resolution/dependency-cycles" should "be compiled with 'inheritance cycle' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/dependency-cycles.lore")(
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
