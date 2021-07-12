package lore.compiler.phases.resolution

import lore.compiler.semantics.scopes.Scope
import lore.compiler.test.BaseSpec

class TypeDependencySpec extends BaseSpec {

  private val fragmentBase = "phases/resolution"

  "resolution/dependency-cycles" should "be compiled with 'inheritance cycle' errors" in {
    assertCompilationErrors(s"$fragmentBase/dependency-cycles.lore") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[TypeDependencies.InheritanceCycle], 1),
        ErrorSignature(classOf[TypeDependencies.InheritanceCycle], 1),
        ErrorSignature(classOf[TypeDependencies.InheritanceCycle], 2),
        ErrorSignature(classOf[TypeDependencies.InheritanceCycle], 2),
        ErrorSignature(classOf[TypeDependencies.InheritanceCycle], 3),
        ErrorSignature(classOf[Scope.UnknownEntry], 4),
        ErrorSignature(classOf[TypeDependencies.InheritanceCycle], 9),
        ErrorSignature(classOf[TypeDependencies.InheritanceCycle], 10),
        ErrorSignature(classOf[TypeDependencies.InheritanceCycle], 13),
        ErrorSignature(classOf[TypeDependencies.InheritanceCycle], 15),
        ErrorSignature(classOf[TypeDependencies.InheritanceCycle], 16),
      ))
    }
  }

}
