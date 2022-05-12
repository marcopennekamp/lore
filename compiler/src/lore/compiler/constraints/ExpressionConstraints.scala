package lore.compiler.constraints

import lore.compiler.feedback.Reporter
import lore.compiler.syntax.TopLevelExprNode

object ExpressionConstraints {

  /**
    * Verifies:
    *   1. Return constraints for the expression.
    */
  def verify(node: TopLevelExprNode)(implicit reporter: Reporter): Unit = {
    ReturnConstraints.verify(node)
  }

}
