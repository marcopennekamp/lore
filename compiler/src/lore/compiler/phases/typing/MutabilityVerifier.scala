package lore.compiler.phases.typing

import lore.compiler.core.{Compilation, Error}
import lore.compiler.core.Compilation.Verification
import lore.compiler.phases.typing.MutabilityVerifier.ImmutableAssignment
import lore.compiler.semantics.expressions.{Expression, ExpressionVerificationVisitor}

/**
  * Verifies that mutable actions (such as assignments) may be performed.
  *
  * TODO: Don't forget to apply this visitor!
  * TODO: Is `typing` the right package name for this?
  */
class MutabilityVerifier extends ExpressionVerificationVisitor {

  override def verify(expression: Expression): Verification = expression match {
    case Expression.Assignment(target, _, _) =>
      if (!target.isMutable) {
        Compilation.fail(ImmutableAssignment(target))
      } else Verification.succeed
    case _ => Verification.succeed
  }

}

object MutabilityVerifier {

  case class ImmutableAssignment(access: Expression.Access) extends Error(access) {
    override def message = s"The variable or member ${access.name} may not be mutated."
  }

}
