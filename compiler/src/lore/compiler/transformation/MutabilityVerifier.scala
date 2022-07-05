package lore.compiler.transformation

import lore.compiler.feedback.{ExpressionFeedback, Reporter}
import lore.compiler.semantics.expressions.{Expression, ExpressionVerificationVisitor}

/**
  * Verifies that mutable actions (such as assignments) may be performed.
  */
class MutabilityVerifier(implicit reporter: Reporter) extends ExpressionVerificationVisitor {
  override def verify(expression: Expression): Unit = expression match {
    case Expression.Assignment(target, _, _) if !target.isMutable =>
      reporter.error(ExpressionFeedback.ImmutableAssignment(target))
    case _ =>
  }
}
