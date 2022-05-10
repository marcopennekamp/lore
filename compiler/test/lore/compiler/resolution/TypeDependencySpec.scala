package lore.compiler.resolution

import lore.compiler.feedback.ScopeFeedback
import lore.compiler.test.BaseSpec

class TypeDependencySpec extends BaseSpec {

  private val fragmentBase = "resolution"

  "resolution/dependency-cycles" should "be compiled with 'inheritance cycle' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/dependency_cycles.lore")(
      (classOf[TypeDependencies.InheritanceCycle], 1),
      (classOf[TypeDependencies.InheritanceCycle], 1),
      (classOf[TypeDependencies.InheritanceCycle], 2),
      (classOf[TypeDependencies.InheritanceCycle], 2),
      (classOf[TypeDependencies.InheritanceCycle], 3),
      (classOf[ScopeFeedback.UnknownEntry], 4),
      (classOf[TypeDependencies.InheritanceCycle], 9),
      (classOf[TypeDependencies.InheritanceCycle], 10),
      (classOf[TypeDependencies.InheritanceCycle], 13),
      (classOf[TypeDependencies.InheritanceCycle], 15),
      (classOf[TypeDependencies.InheritanceCycle], 16),
    )
  }

}
