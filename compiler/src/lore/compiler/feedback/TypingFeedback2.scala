package lore.compiler.feedback

import lore.compiler.core.Positioned
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.{FunctionType, TupleType, Type}

object TypingFeedback2 {

  object MultiFunctions {
    case class FunctionTypeExpected(expression: Expression.MultiFunctionValue) extends Feedback.Error(expression) {
      override def message: String = s"The multi-function value cannot be coerced without a proper type context." +
        s" Please provide a function type in an outer expression."
    }

    case class AmbiguousArgumentTypes(mf: MultiFunctionDefinition, candidates: Vector[Type], context: Expression) extends Feedback.Error(context) {
      override def message: String = s"In this call of multi-function $mf, the argument types cannot be inferred. There" +
        s" are multiple equally specific candidates. These are: ${candidates.mkString(", ")}."
    }
  }

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

  object Lists {
    case class ListExpected(expression: Expression.BinaryOperation, actualType: Type) extends Feedback.Error(expression) {
      override def message: String = s"You can only append elements to lists. The type $actualType is not a list."
    }
  }

  object ValueCalls {
    case class FunctionExpected(expression: Expression.Call, actualType: Type) extends Feedback.Error(expression) {
      override def message: String = s"Only functions may be called. You are trying to call a value of type $actualType."
    }

    case class IllegalArity(expression: Expression.Call, inputType: TupleType) extends Feedback.Error(expression) {
      override def message: String = s"A function of arity ${inputType.elements.length} cannot be called with ${expression.arguments.length} arguments."
    }
  }

  object Loops {
    case class CollectionExpected(actualType: Type, context: Positioned) extends Feedback.Error(context) {
      override def message: String = s"You can only iterate over lists and maps. The type $actualType is not a list or map."
    }
  }

}
