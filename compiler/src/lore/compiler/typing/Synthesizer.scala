package lore.compiler.typing

import lore.compiler.feedback._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.bindings.LocalVariable
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.typed.Expression._
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression._
import lore.compiler.semantics.expressions.{BinaryOperator, UnaryOperator, XaryOperator}
import lore.compiler.types.{BasicType, Type}
import lore.compiler.utils.CollectionExtensions.{Tuple2OptionExtension, VectorExtension}

object Synthesizer {

  /**
    * Infers the type of `expression` solely from the shape of the expression and the inference context, producing a
    * typed expression. If the type cannot be inferred, one or more errors are reported and `None` is returned.
    */
  def infer(
    expression: UntypedExpression,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    def simpleResult(expression: Expression) = Some(expression, context)

    val result: Option[InferenceResult] = expression match {
      case UntypedHole(tpe, position) => simpleResult(Hole(tpe, position))

      case UntypedTypeAscription(value, expectedType, position) =>
        Checker.check(value, expectedType, context).mapFirst {
          typedValue => TypeAscription(typedValue, expectedType, position)
        }

      case UntypedIntValue(value, position) => simpleResult(IntValue(value, position))
      case UntypedRealValue(value, position) => simpleResult(RealValue(value, position))
      case UntypedBooleanValue(value, position) => simpleResult(BooleanValue(value, position))
      case UntypedStringValue(value, position) => simpleResult(StringValue(value, position))
      case UntypedSymbolValue(name, position) => simpleResult(SymbolValue(name, position))

      case UntypedTupleValue(elements, position) => infer(elements, context).mapFirst(TupleValue(_, position))

      case expression@UntypedLambdaValue(parameters, body, position) =>
        // To infer the type of a lambda function, its parameters must be fully annotated.
        if (expression.isFullyAnnotated) {
          val parameterTypes = parameters.map(_.typeAnnotation.get)
          val (typedParameters, context2) = LambdaTyping.buildTypedParameters(parameters, parameterTypes, context)
          infer(body, context2).mapFirst(LambdaValue(typedParameters, _, position))
        } else {
          reporter.report(TypingFeedback.Lambda.TypeContextExpected(expression))
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
          case UnaryOperator.Negation => OperationTyping.checkArithmeticOperand(operand, context).mapFirst(
            typedOperand => create(typedOperand, typedOperand.tpe)
          )
          case UnaryOperator.LogicalNot => Checker.check(operand, BasicType.Boolean, context).mapFirst(create(_, BasicType.Boolean))
        }

      case operation: UntypedBinaryOperation => operation.operator match {
        case BinaryOperator.Addition | BinaryOperator.Subtraction | BinaryOperator.Multiplication | BinaryOperator.Division =>
          OperationTyping.inferArithmeticOperation(operation, context)

        case BinaryOperator.Equals | BinaryOperator.LessThan | BinaryOperator.LessThanEquals =>
          OperationTyping.inferComparison(operation, context)

        case BinaryOperator.Append => OperationTyping.inferAppend(operation, context)
      }

      case operation@UntypedXaryOperation(operator, operands, position) => operator match {
        case XaryOperator.Conjunction | XaryOperator.Disjunction =>
          Checker.check(operands, BasicType.Boolean, context)
            .mapFirst(XaryOperation(operator, _, BasicType.Boolean, position))

        case XaryOperator.Concatenation => OperationTyping.inferConcatenation(operation, context)
      }

      case expression: UntypedMultiFunctionCall => MultiFunctionTyping.checkOrInferCall(expression, None, context)

      case expression: UntypedAmbiguousMultiFunctionCall =>
        MultiFunctionTyping.checkOrInferAmbiguousCall(expression, None, context)

      case expression: UntypedConstructorCall => ConstructorTyping.checkOrInferCall(expression, None, context)
      case expression: UntypedValueCall => UniformCallSyntaxTyping.checkOrInferValueCall(expression, None, context)

      case UntypedIntrinsicCall(target, arguments, tpe, position) =>
        infer(arguments, context).mapFirst { typedArguments =>
          IntrinsicCall(target, typedArguments, tpe, position)
        }

      case UntypedVariableDeclaration(variable, value, typeAnnotation, position) =>
        Checker.checkOrInfer(value, typeAnnotation, context).map { case (typedValue, context2) =>
          val typedVariable = LocalVariable(variable, typeAnnotation.getOrElse(typedValue.tpe))
          Typing.logger.trace(s"Local variable: `${typedVariable.name}: ${typedVariable.tpe}`.")
          (
            VariableDeclaration(typedVariable, typedValue, position),
            context2.withLocalVariable(typedVariable),
          )
        }

      case UntypedAssignment(target, value, position) =>
        infer(target, context).flatMap { case (typedTarget: Expression.Access, context2) =>
          if (!typedTarget.isMutable) {
            reporter.error(ExpressionFeedback.ImmutableAssignment(typedTarget))
          }
          Checker.check(value, typedTarget.tpe, context2).mapFirst(Assignment(typedTarget, _, position))
        }

      case expression: UntypedBindingAccess =>
        BindingAccessTyping.checkOrInfer(expression, None, context).flatMap(simpleResult)

      case expression: UntypedMemberAccess => UniformCallSyntaxTyping.checkOrInfer(expression, None, None, context)

      case UntypedReturn(value, position) =>
        Checker.check(value, context.returnType, context).mapFirst(Return(_, position))

      case expression: UntypedBlock => BlockTyping.checkOrInfer(expression, None, context)
      case expression: UntypedCond => CondTyping.checkOrInfer(expression, None, context)
      case expression: UntypedWhileLoop => LoopTyping.checkOrInfer(expression, None, context)
      case expression: UntypedForLoop => LoopTyping.checkOrInfer(expression, None, context)
    }

    result.foreach { case (typedExpression, _) =>
      Typing.traceExpressionType(typedExpression, "Inferred")
    }
    result
  }

  /**
    * Executes [[infer]] for all `expressions` and returns a result if typing has succeeded for all expressions.
    */
  def infer(
    expressions: Vector[UntypedExpression],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResults] = {
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
  )(implicit registry: Registry): (Option[InferenceResult], Vector[Feedback]) = {
    implicit val reporter: MemoReporter = MemoReporter()
    (infer(expression, context), reporter.feedback)
  }

}
