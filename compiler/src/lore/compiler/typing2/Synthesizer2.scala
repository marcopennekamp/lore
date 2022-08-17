package lore.compiler.typing2

import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.BinaryOperator._
import lore.compiler.semantics.expressions.Expression.UnaryOperator.{LogicalNot, Negation}
import lore.compiler.semantics.expressions.Expression.XaryOperator.{Concatenation, Conjunction, Disjunction}
import lore.compiler.semantics.expressions.Expression._
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression._
import lore.compiler.types.{BasicType, TupleType, Type}
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

      // TODO (multi-import): We probably don't need this as a separate case when we move access coercion into the
      //                      typing phase.
      case UntypedMultiFunctionValue(mfs, _) =>
//        // We can infer a multi-function value without a function type context if the multi-function has a single,
//        // monomorphic function.
//        mf.functions match {
//          case Vector(function) if function.isMonomorphic =>
//            MultiFunctionValueSynthesizer.handleFunctionInstance(function.monomorphicInstance, expression, None, assignments)
//
//          case _ =>
//            reporter.error(TypingFeedback.MultiFunctionValue.TypeContextExpected(expression))
//            None
//        }
        ???

      case UntypedFixedFunctionValue(instance, position) => simpleResult(FixedFunctionValue(instance, position))

      // TODO (multi-import): We probably don't need this as a separate case when we move access coercion into the
      //                      typing phase.
      case expression@UntypedConstructorValue(binding, position) =>
        if (binding.isConstant) {
          simpleResult(ConstructorValue(binding, binding.underlyingType, position))
        } else {
          reporter.report(TypingFeedback.ConstructorValue.TypeContextExpected(expression))
          None
        }

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
//        MultiFunctionCallSynthesizer.infer(mf, expression, assignments)
        ???

      case expression: UntypedValueCall =>
//        infer(target, assignments).flatMap { targetAssignments =>
//          val argumentsResult = InferenceVariable.instantiateCandidate(target, targetAssignments) match {
//            case FunctionType(input, output) =>
//              if (arguments.length == input.elements.length) {
//                val assignments3 = arguments.zip(input.elements).foldSome(targetAssignments) {
//                  case (assignments2, (argument, parameterType)) =>
//                    checker.check(argument, parameterType, assignments2)
//                }
//                assignments3.map((_, output))
//              } else {
//                reporter.error(TypingFeedback.Function.IllegalArity(expression.arguments.length, input.elements.length, expression))
//                None
//              }
//
//            case targetType =>
//              reporter.error(TypingFeedback.ValueCall.FunctionExpected(expression, targetType))
//              None
//          }
//          argumentsResult.flatMap {
//            case (argumentAssignments, output) => Unification.unifyEquals(tpe, output, argumentAssignments)
//          }
//        }
        ???

      case expression: UntypedConstructorCall =>
//        ConstructorCallSynthesizer.infer(structBinding, expression, assignments)
        ???

      case UntypedIntrinsicCall(target, arguments, tpe, position) =>
        infer(arguments, context).mapFirst { typedArguments =>
          IntrinsicCall(target, typedArguments, tpe, position)
        }

      case _: UntypedVariableDeclaration => delegate(TupleType.UnitType)
      case _: UntypedAssignment => delegate(TupleType.UnitType)

      case UntypedBindingAccess(binding, position) =>
        // TODO (multi-import): Don't forget to move access coercion here...
        ???

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
    expressions.foldSome((Vector.empty[Expression], context)) {
      case ((typedExpressions, context), expression) =>
        infer(expression, context).map {
          case (typedExpression, context2) => (typedExpressions :+ typedExpression, context2)
        }
    }
  }

}
