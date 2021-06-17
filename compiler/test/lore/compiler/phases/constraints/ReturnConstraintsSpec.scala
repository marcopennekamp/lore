package lore.compiler.phases.constraints

import lore.compiler.test.BaseSpec

class ReturnConstraintsSpec extends BaseSpec {
  private val fragmentBase = "phases/constraints"

  "constraints/returns" should "be compiled with 'dead code' and 'impossible return' errors" in {
    assertCompilationErrors(s"$fragmentBase/returns.lore") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[ReturnConstraints.ImpossibleReturn], 3),
        ErrorSignature(classOf[ReturnConstraints.DeadCode], 13),
      ))
    }
  }
}
