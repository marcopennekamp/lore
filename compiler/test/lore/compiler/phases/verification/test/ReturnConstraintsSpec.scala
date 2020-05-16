package lore.compiler.phases.verification.test

import lore.compiler.phases.verification.ReturnConstraints
import lore.compiler.test.BaseSpec

class ReturnConstraintsSpec extends BaseSpec {
  "The compiler" should "compile return-constraints.lore with 'dead code' and 'impossible return' errors" in {
    assertCompilationErrors("test/verification/return-constraints") { errors =>
      assertErrorsMatchSignatures(errors, List(
        ErrorSignature(classOf[ReturnConstraints.ImpossibleReturn], 5),
        ErrorSignature(classOf[ReturnConstraints.DeadCode], 14),
      ))
    }
  }
}
