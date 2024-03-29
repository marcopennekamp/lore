package lore.compiler.feedback

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.definitions.BindingDefinition
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.untyped.UntypedExpression._
import lore.compiler.semantics.functions.{FunctionIdentity, FunctionSignature, MultiFunctionDefinition}
import lore.compiler.semantics.modules.MultiReference
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.types._
import lore.compiler.typing.CallTyping.UntypedOrTypedExpression

object TypingFeedback {

  case class SubtypeExpected(
    actualType: Type,
    expectedType: Type,
    positioned: Positioned,
  ) extends Feedback.Error(positioned) {
    override def message: String = s"`${positioned.position.truncatedCode}` has the illegal type `$actualType`. We" +
      s" expected the following type (or a subtype thereof): $expectedType."
  }

  case class IllegalBounds(
    typeArgument: Type,
    typeParameterName: Option[String],
    lowerBound: Type,
    upperBound: Type,
    positioned: Positioned,
  ) extends Feedback.Error(positioned) {
    override def message: String = {
      val typeParameterInfo = typeParameterName.map(name => s" of type parameter `$name`").getOrElse("")
      s"The type argument `$typeArgument` must adhere to the lower bound `$lowerBound` and the upper bound" +
        s" `$upperBound`$typeParameterInfo."
    }
  }

  object IllegalBounds {
    def apply(typeArgument: Type, tv: TypeVariable, position: Position): IllegalBounds = {
      IllegalBounds(typeArgument, Some(tv.simpleName), tv.lowerBound, tv.upperBound, position)
    }
  }

  case class InvalidVariance(
    typeVariable: TypeVariable,
    origin: Variance,
    positioned: Positioned,
  ) extends Feedback.Error(positioned) {
    override def message: String = s"The ${typeVariable.variance.humanReadable} type variable `$typeVariable` is in an" +
      s" illegal ${origin.humanReadable} position."
  }

  case class AmbiguousMultiReference(
    multiReference: MultiReference[BindingDefinition],
    localCandidates: Vector[BindingDefinition],
    globalCandidates: Vector[BindingDefinition],
    positioned: Positioned,
  ) extends Feedback.Error(positioned) {
    override def message: String = {
      val label = multiReference.definitionKind.label
      val candidateInfo = if (localCandidates.isEmpty && globalCandidates.isEmpty) {
        s"None of the following locally or globally available ${label}s were valid candidates:\n" +
          s"${makeCandidateList(multiReference.local, multiReference.global)}"
      } else {
        s"The following ${label}s are valid candidates:\n${makeCandidateList(localCandidates, globalCandidates)}" +
          s" \nGlobally available ${label}s are tried after locally available ${label}s."
      }
      s"The $label `${multiReference.simpleName}` is available from multiple modules and the specific $label to use" +
        s" cannot be disambiguated by type. $candidateInfo"
    }

    private def makeCandidateList(local: Vector[BindingDefinition], global: Vector[BindingDefinition]): String = {
      val localStrings = local.map(definition => s"  - ${definition.name} (local)")
      val globalStrings = global.map(definition => s"  - ${definition.name} (global)")
      (localStrings ++ globalStrings).mkString("\n")
    }
  }

  object Schema {
    case class IllegalArity(
      schema: TypeSchema,
      arity: Int,
      positioned: Positioned,
    ) extends Feedback.Error(positioned) {
      override def message: String = s"The type `$schema` expects ${schema.schemaArity} type arguments, but $arity type" +
        s" arguments were supplied."
    }

    case class ConstantUseRequired(tpe: Type, positioned: Positioned) extends Feedback.Error(positioned) {
      override def message: String = s"The type `$tpe` is constant and does not allow any type arguments."
    }
  }

  object MultiFunctionValue {
    case class IllegalOutput(
      mf: MultiFunctionDefinition,
      actualType: FunctionType,
      expectedType: FunctionType,
      positioned: Positioned,
    ) extends Feedback.Error(positioned) {
      override def message: String = s"While coercing the multi-function `${mf.name}` to a function, the following" +
        s" function type was expected: $expectedType. The actual function type inferred via dispatch is `$actualType`." +
        s" The multi-function cannot be coerced to the expected function type because the output types are" +
        s" incompatible."
    }

    case class FunctionTypeExpected(
      mf: MultiFunctionDefinition,
      expectedType: Type,
      positioned: Positioned,
    ) extends Feedback.Error(positioned) {
      override def message: String = s"The multi-function `${mf.name}` can only be coerced to a function type. The" +
        s" expected type is `$expectedType`, which is not a function type. Most likely, the multi-function cannot be" +
        s" used as a value in this context."
    }

    case class TypeContextExpected(
      mf: MultiFunctionDefinition,
      positioned: Positioned,
    ) extends Feedback.Error(positioned) {
      override def message: String = s"The multi-function `${mf.name}` cannot be coerced to a function value without a" +
        s" proper type context. Please provide a function type in an outer expression (e.g. with a type ascription)."
    }
  }

  object ConstructorValue {
    case class FunctionTypeExpected(
      expression: UntypedBindingAccess,
      expectedType: Type,
    ) extends Feedback.Error(expression) {
      override def message: String = s"A constructor can only be coerced to a function type. The expected type is" +
        s" `$expectedType`, which is not a function type. Most likely, the constructor cannot be used as a value in" +
        s" this context."
    }

    case class TypeContextExpected(expression: UntypedBindingAccess) extends Feedback.Error(expression) {
      override def message: String = s"The constructor cannot be coerced to a function without a proper type context." +
        s" Please provide a function type in an outer expression (e.g. with a type ascription)."
    }

    case class IllegalArity(
      signature: FunctionSignature,
      expectedType: FunctionType,
      positioned: Positioned,
    ) extends Feedback.Error(positioned) {
      override def message: String = s"A constructor with ${signature.arity} parameter cannot be coerced to a function" +
        s" type `$expectedType`. The function type's arity must match the constructor's arity."
    }
  }

  object Tuple {
    case class IncorrectLength(expression: UntypedTupleValue, expectedType: TupleType) extends Feedback.Error(expression) {
      override def message: String = s"The tuple has ${expression.elements.length} elements, but the expected tuple type" +
        s" `$expectedType` requires ${expectedType.elements.length} elements."
    }
  }

  object Lambda {
    case class FunctionTypeExpected(
      expression: UntypedLambdaValue,
      expectedType: Type,
    ) extends Feedback.Error(expression) {
      override def message: String = s"The type of the lambda function cannot be inferred from a type `$expectedType`." +
        s" Either annotate all parameters with a type, or provide a function type in an outer expression."
    }

    case class IllegalArity(
      expression: UntypedLambdaValue,
      expectedType: FunctionType,
    ) extends Feedback.Error(expression) {
      override def message: String = s"The lambda function declares ${expression.arity} parameters, but the expected" +
        s" function type `$expectedType` expects ${expectedType.arity} parameters."
    }

    case class IllegalParameterType(
      expectedParameterType: Type,
      parameterType: Type,
      parameterPosition: Position,
    ) extends Feedback.Error(parameterPosition) {
      override def message: String = s"The lambda function declares a parameter of type `$parameterType`, but the" +
        s" expected function type expects `$expectedParameterType` for this parameter."
    }

    case class TypeContextExpected(expression: UntypedLambdaValue) extends Feedback.Error(expression) {
      override def message: String = s"The type of the lambda function cannot be inferred. Either annotate all" +
        s" parameters with a type, or provide a function type in an outer expression."
    }
  }

  object Shape {
    case class DuplicateProperty(node: TypeExprNode.ShapePropertyNode) extends Feedback.Error(node) {
      override def message: String = s"The property `${node.name}` is declared twice in the shape type. Shape type" +
        s" properties must be unique."
    }
  }

  object Append {
    case class ListExpected(collection: Expression, positioned: Positioned) extends Feedback.Error(positioned) {
      override def message: String = s"You can only append elements to lists. The collection of type" +
        s" `${collection.tpe}` is not a list."
    }
  }

  object Call {
    case class IllegalArity(
      function: FunctionIdentity,
      argumentCount: Int,
      positioned: Positioned,
    ) extends Feedback.Error(positioned) {
      override def message: String = s"A function `$function` with ${function.arity} parameters cannot be called with" +
        s" $argumentCount arguments."
    }
  }

  object MultiFunctionCall {
    case class AmbiguousArgumentTypes(
      mf: MultiFunctionDefinition,
      inputTypes: Vector[Type],
      positioned: Positioned,
    ) extends Feedback.Error(positioned) {
      override def message: String = s"In this call of multi-function `${mf.name}`, the argument types cannot be" +
        s" inferred. There are multiple equally specific candidates. These are: ${inputTypes.mkString(", ")}."
    }
  }

  object ValueCall {
    case class FunctionExpected(actualType: Type, positioned: Positioned) extends Feedback.Error(positioned) {
      override def message: String = s"Only functions may be called. You are trying to call a value of type `$actualType`."
    }
  }

  object Member {
    case class NotFound(
      expression: UntypedMemberAccess,
      instance: UntypedOrTypedExpression,
    ) extends Feedback.Error(expression) {
      override def message: String = s"`${instance.merge.position.truncatedCode}` does not have a member" +
        s" `${expression.name}`."
    }
  }

  object Loop {
    case class CollectionExpected(actualType: Type, positioned: Positioned) extends Feedback.Error(positioned) {
      override def message: String = s"You can only iterate over lists and maps. The type `$actualType` is not a list" +
        s" or map."
    }
  }

}
