package lore.compiler.feedback

import lore.compiler.core.Positioned
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.UnresolvedMemberAccess
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.scopes.StructConstructorBinding
import lore.compiler.types.{DeclaredType, FunctionType, TupleType, Type}

object TypingFeedback {

  case class SubtypeExpected(actualType: Type, expectedType: Type, context: Positioned) extends Feedback.Error(context) {
    override def message: String = s"This expression has the illegal type $actualType. We expected the following type (or a subtype thereof): $expectedType."
  }

  object Members {
    case class NotFound(expression: UnresolvedMemberAccess, instanceType: Type) extends Feedback.Error(expression) {
      override def message: String = s"The type $instanceType does not have a member ${expression.name}."
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

  object MultiFunctionValues {
    case class FunctionTypeExpected(expression: Expression.MultiFunctionValue, expectedType: Type) extends Feedback.Error(expression) {
      override def message: String = s"A multi-function can only be coerced to a function type. The expected type is" +
        s" $expectedType, which is not a function type. Most likely, the multi-function ${expression.mf.name} cannot be" +
        s" used as a value in this context."
    }

    case class IllegalOutput(expression: Expression.MultiFunctionValue, expectedType: FunctionType, actualType: FunctionType) extends Feedback.Error(expression) {
      override def message: String = s"While coercing the multi-function ${expression.mf.name} to a function, the" +
        s" following function type was expected: $expectedType. The actual function type inferred via dispatch is" +
        s" $actualType. The multi-function cannot be coerced to the expected function type because the output types" +
        s" are incompatible."
    }

    case class TypeContextExpected(expression: Expression.MultiFunctionValue) extends Feedback.Error(expression) {
      override def message: String = s"The multi-function cannot be coerced to a function value without a proper type context." +
        s" Please provide a function type in an outer expression."
    }
  }

  object ConstructorValues {
    case class TypeContextExpected(expression: Expression.UntypedConstructorValue) extends Feedback.Error(expression) {
      override def message: String = s"The constructor value's type arguments cannot be inferred without a proper type context." +
        s" Please provide a function type in an outer expression."
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

  object MultiFunctionCalls {
    case class AmbiguousArgumentTypes(mf: MultiFunctionDefinition, candidates: Vector[Type], context: Expression) extends Feedback.Error(context) {
      override def message: String = s"In this call of multi-function $mf, the argument types cannot be inferred. There" +
        s" are multiple equally specific candidates. These are: ${candidates.mkString(", ")}."
    }
  }

  object ConstructorCalls {
    case class CannotSpecialize(binding: StructConstructorBinding, expectedType: DeclaredType, context: Expression) extends Feedback.Error(context) {
      override def message: String = s"A construction of ${binding.definition.name} cannot result in expected type $expectedType," +
        s" because $expectedType cannot be specialized to ${binding.definition.name}. Most likely, ${binding.definition.name}" +
        s" is not a subtype of $expectedType."
    }
  }

  object Loops {
    case class CollectionExpected(actualType: Type, context: Positioned) extends Feedback.Error(context) {
      override def message: String = s"You can only iterate over lists and maps. The type $actualType is not a list or map."
    }
  }

}
