package lore.compiler.phases.transformation

import lore.compiler.test.BaseSpec

class ConstraintsSpec extends BaseSpec {
  private val fragmentBase = "phases/transformation/constraints"

  // TODO: Cover other constraints so that at least all possible constraint errors are tested with at least one
  //       test source each.

  "constraints/return-types" should "be compiled with various errors" in {
    assertCompilationErrors(s"$fragmentBase/return-types") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 8),
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 11),
      ))
    }
  }

  "constraints/returns" should "be compiled with 'dead code' and 'impossible return' errors" in {
    assertCompilationErrors(s"$fragmentBase/returns") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[ReturnConstraints.ImpossibleReturn], 3),
        ErrorSignature(classOf[ReturnConstraints.DeadCode], 13),
      ))
    }
  }
}
