package lore.compiler.constraints

import lore.compiler.test.BaseSpec

class ReturnConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints"

  s"$fragmentBase/returns" should "be compiled with 'dead code' and 'impossible return' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/returns.lore")(
      (classOf[ReturnConstraints.ImpossibleReturn], 3),
      (classOf[ReturnConstraints.DeadCode], 13),
    )
  }
}
