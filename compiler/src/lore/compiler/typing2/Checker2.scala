package lore.compiler.typing2

import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.bindings.LocalVariable
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression._
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression._
import lore.compiler.types.{ListType, ShapeType, TupleType, Type}
import lore.compiler.utils.CollectionExtensions.{Tuple2OptionExtension, VectorExtension}

/**
  * @param returnType The expected return type of the surrounding function, used to check `Return` expressions.
  */
case class Checker2(returnType: Type) {

  private implicit val checker: Checker2 = this

  /**
    * Checks that `expression` has the type `expectedType` (or a subtype thereof). Any typing errors result in `None`.
    *
    * @param expectedType The type expected from the expression by the surrounding context. It informs certain
    *                     inference decisions. TODO (multi-import): "`expectedType` must be fully instantiated." Needed?
    */
  def check(
    expression: UntypedExpression,
    expectedType: Type,
    context: InferenceContext,
  )(implicit reporter: Reporter): Option[InferenceResult] = {
    def fallback = Synthesizer2.infer(expression, context)

    // Step 1: Check and/or infer the untyped expression to produce a typed expression.
    val result: Option[InferenceResult] = expression match {
      case UntypedHole(tpe, position) => Some((Hole(tpe, position), context))

      case UntypedTypeAscription(expression, expectedType, _) => check(expression, expectedType, context)

      case expression@UntypedTupleValue(elements, position) =>
        expectedType match {
          case expectedType@TupleType(elementTypes) =>
            if (elements.length == elementTypes.length) {
              check(elements, elementTypes, context).mapFirst(Tuple(_, position))
            } else {
              reporter.report(TypingFeedback.Tuple.IncorrectLength2(expression, expectedType))
              None
            }

          case _ => fallback
        }

      case expression: UntypedLambdaValue =>
        if (expression.isFullyAnnotated) {
          Synthesizer2.infer(expression, context)
        } else {
          LambdaTyping.checkVagueLambdaValue(expression, expectedType, context)
        }

      // TODO (multi-import): We probably don't need this as a separate case when we move access coercion into the
      //                      typing phase.
      case UntypedMultiFunctionValue(multiReference, position) =>
        /* expectedType match {
          case expectedType@FunctionType(expectedInput, _) =>
            mf.dispatch(
              expectedInput,
              MultiFunctionFeedback.Dispatch.EmptyFit(mf, expectedInput, position),
              min => MultiFunctionFeedback.Dispatch.AmbiguousCall(mf, expectedInput, min, position),
            ) match {
              case Some(instance) => MultiFunctionValueSynthesizer.handleFunctionInstance(
                instance,
                expression,
                Some(expectedType),
                assignments
              )

              case None =>
                // `dispatch` already reported an error.
                None
            }

          case BasicType.Any =>
            // If the expected type isn't a function type, but still can be a supertype of the function type, the
            // Synthesizer may be able to infer the multi-function value if the multi-function contains a single,
            // monomorphic function.
            fallback

          case _ =>
            reporter.error(TypingFeedback.MultiFunctionValue.FunctionTypeExpected(expression, expectedType))
            None
        } */
        ???

      // TODO (multi-import): We probably don't need this as a separate case when we move access coercion into the
      //                      typing phase.
      case UntypedExpression.UntypedConstructorValue(_, _) =>
        /* expectedType match {
          case FunctionType(input, _) =>
            ArgumentSynthesizer.inferTypeArguments(binding.signature, input.elements, assignments, expression).flatMap {
              case ArgumentSynthesizer.Result(assignments2, typeArguments) =>
                InferenceVariable.assign(
                  tpe,
                  binding.instantiateStructType(typeArguments).constructorSignature.functionType,
                  assignments2,
                )
            }

          case _ => fallback
        } */
        ???

      case UntypedListValue(elements, position) =>
        expectedType match {
          case ListType(elementType) =>
            check(elements, elementType, context).mapFirst(ListConstruction(_, position))
          case _ => fallback
        }

      case UntypedShapeValue(properties, position) =>
        expectedType match {
          case shapeType: ShapeType =>
            properties
              .foldSome((Vector.empty[ShapeProperty], context)) {
                case ((typedProperties, context2), property) =>
                  checkOrInfer(property.value, shapeType.propertyType(property.name), context2).mapFirst {
                    typedValue => typedProperties :+ ShapeProperty(property.name, typedValue)
                  }
              }
              .mapFirst(ShapeValue(_, position))
          case _ => fallback
        }

      case UntypedMultiFunctionCall(target, arguments, position) =>
        // MultiFunctionCallChecker.check(mf, expression, expectedType, assignments)
        ???

      case UntypedConstructorCall(target, arguments, position) =>
        // TODO (multi-import): Do we want to use the expected type of a constructor call to help with type parameter
        //                      assignments?
        // expectedType match {
        //   case dt: DeclaredType => ConstructorCallChecker.check(structBinding, expression, dt, assignments)
        //   case _ => fallback
        // }
        ???

      case UntypedVariableDeclaration(variable, value, typeAnnotation, position) =>
        checkOrInfer(value, typeAnnotation, context).map { case (typedValue, context2) =>
          val typedVariable = LocalVariable(variable, typeAnnotation.getOrElse(typedValue.tpe))
          (
            // TODO (multi-import): Do we even need to generate variable declarations or can we just use an assignment?
            VariableDeclaration(typedVariable, typedValue, typeAnnotation, position),
            context2.withLocalVariable(typedVariable),
          )
        }

      case UntypedAssignment(target, value, position) =>
        Synthesizer2.infer(target, context).flatMap { case (typedTarget: Expression.Access, context2) =>
          check(value, typedTarget.tpe, context2).mapFirst(Assignment(typedTarget, _, position))
        }

      case UntypedReturn(value, position) => check(value, returnType, context).mapFirst(Return(_, position))

      case block: UntypedBlock => BlockTyping.checkOrInfer(block, Some(expectedType), context)
      case expression: UntypedCond => CondTyping.checkOrInfer(expression, Some(expectedType), context)
      case expression: UntypedWhileLoop => LoopTyping.checkOrInfer(expression, Some(expectedType), context)
      case expression: UntypedForLoop => LoopTyping.checkOrInfer(expression, Some(expectedType), context)

      case _ => fallback
    }

    result.flatMap { case (typedExpression, _) =>
      Typing2.traceExpressionType(typedExpression, "Checked", s" (Expected type: $expectedType.)")

      // Step 2: Check that the typed expression agrees with the expected type.
      if (typedExpression.tpe </= expectedType) {
        reporter.error(TypingFeedback.SubtypeExpected(typedExpression.tpe, expectedType, expression))
        None
      } else {
        result
      }
    }
  }

  /**
    * Executes [[check]] for all `expressions` and returns a result if typing has succeeded for all expressions.
    */
  def check(
    expressions: Vector[UntypedExpression],
    expectedType: Type,
    context: InferenceContext,
  )(implicit reporter: Reporter): Option[InferenceResults] = {
    check(expressions, Vector.fill(expressions.length)(expectedType), context)
  }

  /**
    * Executes [[check]] for all pairs of `expressions` and `expectedTypes` and returns a result if typing has
    * succeeded for all expressions.
    */
  def check(
    expressions: Vector[UntypedExpression],
    expectedTypes: Vector[Type],
    context: InferenceContext,
  )(implicit reporter: Reporter): Option[InferenceResults] = {
    expressions.zip(expectedTypes).foldSome((Vector.empty[Expression], context)) {
      case ((typedExpressions, context), (expression, expectedType)) =>
        check(expression, expectedType, context).map {
          case (typedExpression, context2) => (typedExpressions :+ typedExpression, context2)
        }
    }
  }

  /**
    * Depending on whether `expectedType` is defined, performs [[check]] or [[Synthesizer2.infer]].
    */
  // noinspection DuplicatedCode
  def checkOrInfer(
    expression: UntypedExpression,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit reporter: Reporter): Option[InferenceResult] = expectedType match {
    case Some(expectedType) => check(expression, expectedType, context)
    case None => Synthesizer2.infer(expression, context)
  }

  /**
    * Depending on whether `expectedType` is defined, performs [[check]] or [[Synthesizer2.infer]].
    */
  // noinspection DuplicatedCode
  def checkOrInfer(
    expressions: Vector[UntypedExpression],
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit reporter: Reporter): Option[InferenceResults] = expectedType match {
    case Some(expectedType) => check(expressions, expectedType, context)
    case None => Synthesizer2.infer(expressions, context)
  }

}
