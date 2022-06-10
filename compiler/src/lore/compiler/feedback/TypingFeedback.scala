package lore.compiler.feedback

import lore.compiler.core.Positioned
import lore.compiler.semantics.bindings.StructConstructorBinding
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.UnresolvedMemberAccess
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.{DeclaredType, FunctionType, TupleType, Type}

object TypingFeedback {

  case class SubtypeExpected(actualType: Type, expectedType: Type, context: Positioned) extends Feedback.Error(context) {
    override def message: String = s"This expression has the illegal type `$actualType`. We expected the following type" +
      s" (or a subtype thereof): $expectedType."
  }

  object Members {
    case class NotFound(expression: UnresolvedMemberAccess, instanceType: Type) extends Feedback.Error(expression) {
      override def message: String = s"The type `$instanceType` does not have a member `${expression.name}`."
    }
  }

  object Functions {
    case class IllegalArity(argumentCount: Int, parameterCount: Int, context: Positioned) extends Feedback.Error(context) {
      override def message: String = s"A function with $parameterCount parameters received $argumentCount arguments."
    }

    case class IllegalArgumentTypes(argumentTypes: Vector[Type], parameterTypes: Vector[Type], context: Positioned) extends Feedback.Error(context) {
      override def message: String = s"The argument types `(${argumentTypes.mkString(", ")})` don't fit into the expected" +
        s" parameter types `(${parameterTypes.mkString(", ")})`."
    }

    case class IllegalTypeArguments(typeArguments: Vector[Type], typeParameters: Vector[Type], context: Positioned) extends Feedback.Error(context) {
      override def message: String = s"The type arguments `${typeArguments.mkString(", ")}` don't fit the type" +
        s" parameters `${typeParameters.mkString(", ")}`."
    }
  }

  object AnonymousFunctions {
    case class FunctionTypeExpected(expression: Expression.AnonymousFunction, expectedType: Type) extends Feedback.Error(expression) {
      override def message: String = s"The type of the anonymous function cannot be inferred from a type `$expectedType`." +
        s" Either annotate all parameters with a type, or provide a function type in an outer expression."
    }

    case class IllegalArity(expression: Expression.AnonymousFunction, expectedType: FunctionType) extends Feedback.Error(expression) {
      override def message: String = s"The anonymous function declares ${expression.parameters.length} parameters, but" +
        s" the expected function type `$expectedType` expects ${expectedType.input.elements.length} parameters."
    }

    case class IllegalParameterTypes(expression: Expression.AnonymousFunction, expectedType: FunctionType, parameterTypes: TupleType) extends Feedback.Error(expression) {
      override def message: String = s"The anonymous function declares parameters of type `$parameterTypes`, but the" +
        s" expected function type `$expectedType` has incompatible parameters."
    }

    case class TypeContextExpected(expression: Expression.AnonymousFunction) extends Feedback.Error(expression) {
      override def message: String = s"The type of the anonymous function cannot be inferred. Either annotate all" +
        s" parameters with a type, or provide a function type in an outer expression."
    }
  }

  object MultiFunctionValues {
    case class FunctionTypeExpected(expression: Expression.MultiFunctionValue, expectedType: Type) extends Feedback.Error(expression) {
      override def message: String = s"A multi-function can only be coerced to a function type. The expected type is" +
        s" `$expectedType`, which is not a function type. Most likely, the multi-function `${expression.mf.name}`" +
        s" cannot be used as a value in this context."
    }

    case class IllegalOutput(expression: Expression.MultiFunctionValue, expectedType: FunctionType, actualType: FunctionType) extends Feedback.Error(expression) {
      override def message: String = s"While coercing the multi-function `${expression.mf.name}` to a function, the" +
        s" following function type was expected: $expectedType. The actual function type inferred via dispatch is" +
        s" `$actualType`. The multi-function cannot be coerced to the expected function type because the output types" +
        s" are incompatible."
    }

    case class TypeContextExpected(expression: Expression.MultiFunctionValue) extends Feedback.Error(expression) {
      override def message: String = s"The multi-function cannot be coerced to a function value without a proper type" +
        s" context. Please provide a function type in an outer expression (e.g. with a type ascription)."
    }
  }

  object ConstructorValues {
    case class TypeContextExpected(expression: Expression.UntypedConstructorValue) extends Feedback.Error(expression) {
      override def message: String = s"The constructor value's type arguments cannot be inferred without a proper type" +
        s" context. Please provide a function type in an outer expression (e.g. with a type ascription)."
    }
  }

  object Tuples {
    case class IncorrectLength(expression: Expression.Tuple, expectedType: TupleType) extends Feedback.Error(expression) {
      override def message: String = s"The tuple has ${expression.values.length} elements, but the expected tuple type" +
        s" `$expectedType` requires ${expectedType.elements.length} elements."
    }
  }

  object Lists {
    case class ListExpected(expression: Expression.BinaryOperation, actualType: Type) extends Feedback.Error(expression) {
      override def message: String = s"You can only append elements to lists. The type `$actualType` is not a list."
    }
  }

  object ValueCalls {
    case class FunctionExpected(expression: Expression.Call, actualType: Type) extends Feedback.Error(expression) {
      override def message: String = s"Only functions may be called. You are trying to call a value of type `$actualType`."
    }
  }

  object MultiFunctionCalls {
    case class AmbiguousArgumentTypes(mf: MultiFunctionDefinition, candidates: Vector[Type], context: Expression) extends Feedback.Error(context) {
      override def message: String = s"In this call of multi-function `$mf.name`, the argument types cannot be inferred." +
        s" There are multiple equally specific candidates. These are: ${candidates.mkString(", ")}."
    }
  }

  object ConstructorCalls {
    case class CannotSpecialize(binding: StructConstructorBinding, expectedType: DeclaredType, context: Expression) extends Feedback.Error(context) {
      override def message: String = s"A construction of `${binding.definition.name}` cannot result in expected type" +
        s" `$expectedType`, because `$expectedType` cannot be specialized to `${binding.definition.name}`. Most likely," +
        s" `${binding.definition.name}` is not a subtype of `$expectedType`."
    }
  }

  object Loops {
    case class CollectionExpected(actualType: Type, context: Positioned) extends Feedback.Error(context) {
      override def message: String = s"You can only iterate over lists and maps. The type `$actualType` is not a list or map."
    }
  }

}
