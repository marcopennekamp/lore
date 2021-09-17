package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.FunctionType

object ExpressionFeedback {
  case class IllegalSymbolComparison(override val position: Position) extends Feedback.Error(position) {
    override def message = "Symbols are unordered and may not be compared using 'less than' or 'greater than'."
  }

  case class ImmutableAssignment(access: Expression.Access) extends Feedback.Error(access) {
    override def message = s"The variable or member $access may not be mutated."
  }

  object ValueCall {
    case class IllegalArity(target: Expression, targetType: FunctionType, argumentCount: Int, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"A function $target of arity ${targetType.input.elements.length} cannot be called with $argumentCount arguments."
    }
  }
}
