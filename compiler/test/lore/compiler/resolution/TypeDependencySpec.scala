package lore.compiler.resolution

import lore.compiler.test.BaseSpec
import lore.compiler.utils.CollectionExtensions.VectorExtension

class TypeDependencySpec extends BaseSpec {

  private val fragmentBase = "resolution"

  s"$fragmentBase/dependency-cycles" should "be compiled with 'inheritance cycle' errors" in {
    // The errors that the compiler produces vary from run to run, as inheritance cycle analysis is not deterministic.
    // Hence, we are simply checking whether all lines on which errors occur are reported correctly.
    assertCompilationErrors(s"$fragmentBase/dependency_cycles.lore") { errors =>
      val errorLines = errors.filterType[TypeDependencies.InheritanceCycle].map(_.position.startLine).toSet
      errorLines shouldEqual Set(1, 2, 3, 9, 10, 13, 15, 16)
    }
  }

}
