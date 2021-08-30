package lore.compiler.transformation

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.expressions.{Expression, ExpressionVerificationVisitor}
import lore.compiler.transformation.MutabilityVerifier.ImmutableAssignment

/**
  * Verifies that mutable actions (such as assignments) may be performed.
  */
class MutabilityVerifier(implicit reporter: Reporter) extends ExpressionVerificationVisitor {

  override def verify(expression: Expression): Unit = expression match {
    case Expression.Assignment(target, _, _) => if (!target.isMutable) reporter.error(ImmutableAssignment(target))
    case _ =>
  }

}

object MutabilityVerifier {

  case class ImmutableAssignment(access: Expression.Access) extends Feedback.Error(access) {
    override def message = s"The variable or member ${access.name} may not be mutated."
  }

}
