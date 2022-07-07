package lore.compiler.constraints

import lore.compiler.feedback.ExpressionFeedback
import lore.compiler.test.BaseSpec

class ReturnConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints"

  s"$fragmentBase/returns" should "be compiled with 'dead code' and 'impossible return' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/returns.lore")(
      (classOf[ExpressionFeedback.Return.IllegalReturn], 3),
      (classOf[ExpressionFeedback.Return.DeadCode], 13),
    )
  }
}
