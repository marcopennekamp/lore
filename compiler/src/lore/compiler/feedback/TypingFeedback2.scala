package lore.compiler.feedback

import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{FunctionType, TupleType, Type}

object TypingFeedback2 {

  object AnonymousFunctions {
    case class FunctionTypeExpected(expression: Expression.AnonymousFunction, expectedType: Type) extends Feedback.Error(expression) {
      override def message: String = s"The type of the anonymous function cannot be inferred from a type $expectedType." +
        s" Either annotate all parameters with a type, or provide a function type in an outer expression."
    }

    case class IllegalArity(expression: Expression.AnonymousFunction, expectedType: FunctionType) extends Feedback.Error(expression) {
      override def message: String = s"The anonymous function declares ${expression.parameters.length} parameters, but" +
        s" the expected function type $expectedType expects ${expectedType.input.elements.length} parameters."
    }

    case class IllegalParameterTypes(expression: Expression.AnonymousFunction, expectedType: FunctionType, parameterTypes: TupleType) extends Feedback.Error(expression) {
      override def message: String = s"The anonymous function declares parameters of type $parameterTypes, but the" +
        s" expected function type $expectedType has incompatible parameters."
    }

    case class TypeContextExpected(expression: Expression.AnonymousFunction) extends Feedback.Error(expression) {
      override def message: String = s"The type of the anonymous function cannot be inferred. Either annotate all" +
        s" parameters with a type, or provide a function type in an outer expression."
    }
  }

}
