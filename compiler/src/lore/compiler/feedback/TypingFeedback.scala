package lore.compiler.feedback

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.bindings.StructConstructorBinding
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.{UntypedConstructorValue, UntypedLambdaValue, UntypedMemberAccess, UntypedTupleValue}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.types._

object TypingFeedback {

  case class SubtypeExpected(actualType: Type, expectedType: Type, context: Positioned) extends Feedback.Error(context) {
    override def message: String = s"This expression has the illegal type `$actualType`. We expected the following type" +
      s" (or a subtype thereof): $expectedType."
  }

  case class InvalidVariance(
    typeVariable: TypeVariable,
    origin: Variance,
    override val position: Position,
  ) extends Feedback.Error(position) {
    override val message: String = s"The ${typeVariable.variance.humanReadable} type variable `$typeVariable` is in an" +
      s" illegal ${origin.humanReadable} position."
  }

  object Schema {
    case class IllegalArity(
      schema: TypeSchema,
      arity: Int,
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = s"The type `$schema` expects ${schema.arity} type arguments, but $arity type" +
        s" arguments were supplied."
    }

    case class ConstantUseRequired(tpe: Type, context: Positioned) extends Feedback.Error(context) {
      override def message: String = s"The type `$tpe` is constant and does not allow any type arguments."
    }

    case class IllegalBounds(
      tv: TypeVariable,
      argument: Type,
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = s"The type argument `$argument` must adhere to the lower bound `${tv.lowerBound}`" +
        s" and the upper bound `${tv.upperBound}`."
    }
  }

  object Member {
    case class NotFound(expression: UntypedMemberAccess, instanceType: Type) extends Feedback.Error(expression) {
      override def message: String = s"The type `$instanceType` does not have a member `${expression.name}`."
    }
  }

  object Function {
    case class IllegalArity(
      argumentCount: Int,
      parameterCount: Int,
      context: Positioned,
    ) extends Feedback.Error(context) {
      override def message: String = s"A function with $parameterCount parameters received $argumentCount arguments."
    }

    case class IllegalArgumentTypes(
      argumentTypes: Vector[Type],
      parameterTypes: Vector[Type],
      context: Positioned,
    ) extends Feedback.Error(context) {
      override def message: String = s"The argument types `(${argumentTypes.mkString(", ")})` don't fit into the expected" +
        s" parameter types `(${parameterTypes.mkString(", ")})`."
    }

    case class IllegalTypeArguments(
      typeArguments: Vector[Type],
      typeParameters: Vector[Type],
      context: Positioned,
    ) extends Feedback.Error(context) {
      override def message: String = s"The type arguments `${typeArguments.mkString(", ")}` don't fit the type" +
        s" parameters `${typeParameters.mkString(", ")}`."
    }
  }

  object AnonymousFunction {
    case class FunctionTypeExpected(
      expression: Expression.LambdaValue,
      expectedType: Type,
    ) extends Feedback.Error(expression) {
      override def message: String = s"The type of the anonymous function cannot be inferred from a type `$expectedType`." +
        s" Either annotate all parameters with a type, or provide a function type in an outer expression."
    }

    case class FunctionTypeExpected2(
      expression: UntypedLambdaValue,
      expectedType: Type,
    ) extends Feedback.Error(expression) {
      override def message: String = s"The type of the lambda function cannot be inferred from a type `$expectedType`." +
        s" Either annotate all parameters with a type, or provide a function type in an outer expression."
    }

    case class IllegalArity(
      expression: Expression.LambdaValue,
      expectedType: FunctionType,
    ) extends Feedback.Error(expression) {
      override def message: String = s"The anonymous function declares ${expression.parameters.length} parameters, but" +
        s" the expected function type `$expectedType` expects ${expectedType.input.elements.length} parameters."
    }

    case class IllegalArity2(
      expression: UntypedLambdaValue,
      expectedType: FunctionType,
    ) extends Feedback.Error(expression) {
      override def message: String = s"The lambda function declares ${expression.parameters.length} parameters, but" +
        s" the expected function type `$expectedType` expects ${expectedType.input.elements.length} parameters."
    }

    case class IllegalParameterTypes(
      expression: Expression.LambdaValue,
      expectedType: FunctionType,
      parameterTypes: TupleType,
    ) extends Feedback.Error(expression) {
      override def message: String = s"The anonymous function declares parameters of type `$parameterTypes`, but the" +
        s" expected function type `$expectedType` has incompatible parameters."
    }

    case class IllegalParameterType(
      expectedParameterType: Type,
      parameterType: Type,
      parameterPosition: Position,
    ) extends Feedback.Error(parameterPosition) {
      override def message: String = s"The lambda function declares a parameter of type `$parameterType`, but the" +
        s" expected function type expects `$expectedParameterType` for this parameter."
    }

    case class TypeContextExpected(expression: Expression.LambdaValue) extends Feedback.Error(expression) {
      override def message: String = s"The type of the anonymous function cannot be inferred. Either annotate all" +
        s" parameters with a type, or provide a function type in an outer expression."
    }

    case class TypeContextExpected2(expression: UntypedLambdaValue) extends Feedback.Error(expression) {
      override def message: String = s"The type of the anonymous function cannot be inferred. Either annotate all" +
        s" parameters with a type, or provide a function type in an outer expression."
    }
  }

  object MultiFunctionValue {
    case class FunctionTypeExpected(
      expression: Expression.MultiFunctionValue,
      expectedType: Type,
    ) extends Feedback.Error(expression) {
      override def message: String = s"A multi-function can only be coerced to a function type. The expected type is" +
        s" `$expectedType`, which is not a function type. Most likely, the multi-function `${expression.mf.name}`" +
        s" cannot be used as a value in this context."
    }

    case class IllegalOutput(
      expression: Expression.MultiFunctionValue,
      expectedType: FunctionType,
      actualType: FunctionType,
    ) extends Feedback.Error(expression) {
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

  object ConstructorValue {
    case class TypeContextExpected(expression: UntypedConstructorValue) extends Feedback.Error(expression) {
      override def message: String = s"The constructor value's type arguments cannot be inferred without a proper type" +
        s" context. Please provide a function type in an outer expression (e.g. with a type ascription)."
    }
  }

  object Tuple {
    case class IncorrectLength2(expression: UntypedTupleValue, expectedType: TupleType) extends Feedback.Error(expression) {
      override def message: String = s"The tuple has ${expression.elements.length} elements, but the expected tuple type" +
        s" `$expectedType` requires ${expectedType.elements.length} elements."
    }
    case class IncorrectLength(expression: Expression.TupleValue, expectedType: TupleType) extends Feedback.Error(expression) {
      override def message: String = s"The tuple has ${expression.elements.length} elements, but the expected tuple type" +
        s" `$expectedType` requires ${expectedType.elements.length} elements."
    }
  }

  object List {
    case class ListExpected(expression: Expression.BinaryOperation, actualType: Type) extends Feedback.Error(expression) {
      override def message: String = s"You can only append elements to lists. The type `$actualType` is not a list."
    }

    case class AppendListExpected(collection: Expression, positioned: Positioned) extends Feedback.Error(positioned) {
      override def message: String = s"You can only append elements to lists. The collection of type" +
        s" `${collection.tpe}` is not a list."
    }
  }

  object Shape {
    case class DuplicateProperty(node: TypeExprNode.ShapePropertyNode) extends Feedback.Error(node) {
      override def message: String = s"The property `${node.name}` is declared twice in the shape type. Shape type" +
        s" properties must be unique."
    }
  }

  object ValueCall {
    case class FunctionExpected(expression: Expression.Call, actualType: Type) extends Feedback.Error(expression) {
      override def message: String = s"Only functions may be called. You are trying to call a value of type `$actualType`."
    }
  }

  object MultiFunctionCall {
    case class AmbiguousArgumentTypes(
      mf: MultiFunctionDefinition,
      candidates: Vector[Type],
      context: Expression,
    ) extends Feedback.Error(context) {
      override def message: String = s"In this call of multi-function `$mf.name`, the argument types cannot be inferred." +
        s" There are multiple equally specific candidates. These are: ${candidates.mkString(", ")}."
    }
  }

  object ConstructorCall {
    case class CannotSpecialize(
      binding: StructConstructorBinding,
      expectedType: DeclaredType,
      context: Expression,
    ) extends Feedback.Error(context) {
      override def message: String = s"A construction of `${binding.name}` cannot result in expected type" +
        s" `$expectedType`, because `$expectedType` cannot be specialized to `${binding.name}`. Most likely," +
        s" `${binding.name}` is not a subtype of `$expectedType`."
    }
  }

  object Loop {
    case class CollectionExpected(actualType: Type, context: Positioned) extends Feedback.Error(context) {
      override def message: String = s"You can only iterate over lists and maps. The type `$actualType` is not a list" +
        s" or map."
    }
  }

}
