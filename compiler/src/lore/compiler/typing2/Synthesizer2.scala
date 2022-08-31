package lore.compiler.typing2

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Feedback, MemoReporter, Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression.BinaryOperator._
import lore.compiler.semantics.expressions.typed.Expression.UnaryOperator.{LogicalNot, Negation}
import lore.compiler.semantics.expressions.typed.Expression.XaryOperator.{Concatenation, Conjunction, Disjunction}
import lore.compiler.semantics.expressions.typed.Expression._
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression._
import lore.compiler.types.{BasicType, FunctionType, TupleType, Type}
import lore.compiler.typing2.unification.InferenceVariable2
import lore.compiler.utils.CollectionExtensions.{Tuple2OptionExtension, VectorExtension}

object Synthesizer2 {

  /**
    * Infers the type of `expression` solely from the shape of the expression and the inference context, producing a
    * typed expression. If the type cannot be inferred, one or more errors are reported and `None` is returned.
    */
  def infer(
    expression: UntypedExpression,
    context: InferenceContext,
  )(implicit checker: Checker2, registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    def delegate(expectedType: Type) = checker.check(expression, expectedType, context)
    def simpleResult(expression: Expression) = Some(expression, context)

    // Note that some expressions, such as `Return`, are handled by the Checker, so we need to delegate back to it.
    // Because the Synthesizer doesn't know about the expected type, this can only ever be a predetermined one. The
    // other case is returning a "static result", which happens when we try to infer leaf expressions such as literals
    // or binding accesses.
    // To avoid confusion between these two default cases, this match doesn't have a default case.
    val result: Option[InferenceResult] = expression match {
      case UntypedHole(tpe, position) => simpleResult(Hole(tpe, position))

      case UntypedTypeAscription(expression, expectedType, _) => checker.check(expression, expectedType, context)

      case UntypedIntValue(value, position) => simpleResult(IntValue(value, position))
      case UntypedRealValue(value, position) => simpleResult(RealValue(value, position))
      case UntypedBooleanValue(value, position) => simpleResult(BooleanValue(value, position))
      case UntypedStringValue(value, position) => simpleResult(StringValue(value, position))
      case UntypedSymbolValue(name, position) => simpleResult(SymbolValue(name, position))

      case UntypedTupleValue(elements, position) => infer(elements, context).mapFirst(TupleValue(_, position))

      case expression@UntypedLambdaValue(parameters, body, position) =>
        // To infer the type of an anonymous function, its parameters must be fully annotated.
        if (expression.isFullyAnnotated) {
          val parameterTypes = parameters.map(_.typeAnnotation.get)
          val (typedParameters, context2) = LambdaTyping.buildTypedParameters(parameters, parameterTypes, context)
          infer(body, context2).mapFirst(LambdaValue(typedParameters, _, position))
        } else {
          reporter.report(TypingFeedback.AnonymousFunction.TypeContextExpected2(expression))
          None
        }

      case UntypedFixedFunctionValue(instance, position) => simpleResult(FixedFunctionValue(instance, position))

      case UntypedConstructorValue(structType, position) => simpleResult(ConstructorValue(structType, position))

      case UntypedListValue(elements, position) => infer(elements, context).mapFirst(ListValue(_, position))

      case expression: UntypedShapeValue => ShapeTyping.checkOrInfer(expression, None, context)

      case expression: UntypedPropertyDefaultValue =>
        simpleResult(PropertyDefaultValue(expression.property, expression.position))

      case UntypedUnaryOperation(operator, operand, position) =>
        def create(typedOperand: Expression, resultType: Type) = {
          UnaryOperation(operator, typedOperand, resultType, position)
        }

        operator match {
          case Negation => OperationTyping.checkArithmeticOperand(operand, context).mapFirst(
            typedOperand => create(typedOperand, typedOperand.tpe)
          )
          case LogicalNot => checker.check(operand, BasicType.Boolean, context).mapFirst(create(_, BasicType.Boolean))
        }

      case operation: UntypedBinaryOperation => operation.operator match {
        case Addition | Subtraction | Multiplication | Division =>
          OperationTyping.inferArithmeticOperation(operation, context)
        case Equals | LessThan | LessThanEquals => OperationTyping.inferComparison(operation, context)
        case Append => OperationTyping.inferAppend(operation, context)
      }

      case operation@UntypedXaryOperation(operator, operands, position) => operator match {
        case Conjunction | Disjunction =>
          checker.check(operands, BasicType.Boolean, context)
            .mapFirst(XaryOperation(operator, _, BasicType.Boolean, position))

        case Concatenation => OperationTyping.inferConcatenation(operation, context)
      }

      case expression: UntypedMultiFunctionCall =>
        MultiFunctionTyping.checkOrInferCall(expression, None, context)

      case expression: UntypedValueCall =>
        // TODO (multi-import): Once we support uniform call syntax, we additionally need a case in the Checker that
        //                      grabs the expected type should the value call actually be a multi-function call.
        infer(expression.target, context).flatMap { case (typedTarget, context2) =>
          typedTarget.tpe match {
            case targetType: FunctionType =>
              CallTyping.checkOrInfer(targetType, expression, Some(targetType.output), context2)(
                (typedArguments, _) => ValueCall(typedTarget, typedArguments, targetType.output, expression.position)
              )

            case targetType =>
              reporter.error(TypingFeedback.ValueCall.FunctionExpected(expression, targetType))
              None
          }
        }

      case expression: UntypedConstructorCall => ConstructorTyping.checkOrInferCall(expression, None, context)

      case UntypedIntrinsicCall(target, arguments, tpe, position) =>
        infer(arguments, context).mapFirst { typedArguments =>
          IntrinsicCall(target, typedArguments, tpe, position)
        }

      case _: UntypedVariableDeclaration => delegate(TupleType.UnitType)
      case _: UntypedAssignment => delegate(TupleType.UnitType)

      case expression: UntypedBindingAccess =>
        BindingAccessTyping.checkOrInfer(expression, None, context).flatMap(simpleResult)

      case expression@UntypedMemberAccess(instance, name, position) =>
        infer(instance, context).flatMapFirst { typedInstance =>
          typedInstance.tpe.member(name) match {
            case Some(member) => Some(MemberAccess(typedInstance, member, position))
            case None =>
              reporter.report(TypingFeedback.Member.NotFound(expression, typedInstance.tpe))
              None
          }
        }

      case _: UntypedReturn => delegate(BasicType.Nothing)
      case expression: UntypedBlock => BlockTyping.checkOrInfer(expression, None, context)
      case expression: UntypedCond => CondTyping.checkOrInfer(expression, None, context)
      case expression: UntypedWhileLoop => LoopTyping.checkOrInfer(expression, None, context)
      case expression: UntypedForLoop => LoopTyping.checkOrInfer(expression, None, context)
    }

    result.foreach { case (typedExpression, _) =>
      // TODO (multi-import): Temporary/assertion. Remove (in production).
      if (!InferenceVariable2.isFullyInstantiated(typedExpression.tpe)) {
        throw CompilationException("`typedExpression.tpe` must be fully instantiated!")
      }

      Typing2.traceExpressionType(typedExpression, "Inferred")
    }
    result
  }

  /**
    * Executes [[infer]] for all `expressions` and returns a result if typing has succeeded for all expressions.
    */
  def infer(
    expressions: Vector[UntypedExpression],
    context: InferenceContext,
  )(implicit checker: Checker2, registry: Registry, reporter: Reporter): Option[InferenceResults] = {
    expressions.foldSomeCollect(context) {
      case (context, expression) => infer(expression, context)
    }
  }

  /**
    * Attempts type checking via [[infer]], using an internal reporter that accumulates errors, which are then returned
    * separately. `attempt` can be used to try a particular checking path without committing to it.
    */
  def attempt(
    expression: UntypedExpression,
    context: InferenceContext,
  )(implicit checker: Checker2, registry: Registry): (Option[InferenceResult], Vector[Feedback]) = {
    implicit val reporter: MemoReporter = MemoReporter()
    (infer(expression, context), reporter.feedback)
  }

}
