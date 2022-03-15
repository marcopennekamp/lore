package lore.compiler.typing.synthesizer

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Feedback, MemoReporter, Reporter, TypingFeedback}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, UnaryOperator, XaryOperator}
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.types._
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.checker.Checker
import lore.compiler.typing.unification.Unification
import lore.compiler.typing.{InferenceVariable, Typing}
import lore.compiler.utils.CollectionExtensions.VectorExtension

object Synthesizer {

  /**
    * Infers the type of `expression` solely from the shape of the expression and the current inference variable
    * assignments, producing a new set of assignments with which the type of `expression` can be instantiated. If the
    * type cannot be inferred, one or more errors are reported and `None` is returned.
    */
  def infer(expression: Expression, assignments: Assignments)(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    // Delegates the handling of the expression to the Checker.
    def delegate(expectedType: Type) = checker.check(expression, expectedType, assignments)

    // Note that some expressions, such as `Return`, are handled by the Checker, so we need to delegate back to it.
    // Because the Synthesizer doesn't know about the expected type, this can only ever be a predetermined one. The
    // other case is returning a "static result", which happens when we try to infer leaf expressions such as literals
    // or binding accesses.
    // To avoid confusion between these two default cases, this match doesn't have a default case.
    val resultAssignments: Option[Assignments] = expression match {
      case Expression.Hole(_, _) => Some(assignments)

      // These delegations of top-level expressions are necessary, because the inference for blocks, for example, uses
      // inference mode to type its last expression. This expression may well be a return, variable declaration, or
      // assignment.
      case Expression.Return(_, _) => delegate(BasicType.Nothing)
      case Expression.VariableDeclaration(_, _, _, _) => delegate(TupleType.UnitType)
      case Expression.Assignment(_, _, _) => delegate(TupleType.UnitType)

      case block@Expression.Block(expressions, _, _) =>
        checker.check(expressions.init, BasicType.Any, assignments)
          .flatMap(infer(expressions.last, _))
          .flatMap(checker.assignBlockType(block, None, _))

      case Expression.BindingAccess(_, _) => Some(assignments)

      case Expression.MemberAccess(_, _, position) => throw CompilationException(s"At this stage of typechecking, there should be no MemberAccess expressions. Position: $position.")

      case expression@Expression.UnresolvedMemberAccess(instance, name, memberInferenceVariable, _) =>
        infer(instance, assignments).flatMap { assignments2 =>
          val instanceType = InferenceVariable.instantiateCandidate(instance, assignments2)
          instanceType.member(name) match {
            case Some(member) => InferenceVariable.assign(memberInferenceVariable, member.tpe, assignments2)
            case None =>
              reporter.report(TypingFeedback.Members.NotFound(expression, instanceType))
              None
          }
        }

      case Expression.Literal(_, _) => Some(assignments)

      case Expression.Tuple(values, _) => infer(values, assignments)

      case expression@Expression.AnonymousFunction(_, body, _) =>
        // To infer the type of an anonymous function, its parameters must be fully annotated.
        if (expression.isFullyAnnotated) {
          infer(body, assignments)
        } else {
          reporter.report(TypingFeedback.AnonymousFunctions.TypeContextExpected(expression))
          None
        }

      case expression@Expression.MultiFunctionValue(mf, _, _) =>
        // We can infer a multi-function value without a function type context if the multi-function has a single,
        // monomorphic function.
        mf.functions match {
          case Vector(function) if function.isMonomorphic =>
            MultiFunctionValueSynthesizer.handleFunctionInstance(function.monomorphicInstance, expression, None, assignments)

          case _ =>
            reporter.error(TypingFeedback.MultiFunctionValues.TypeContextExpected(expression))
            None
        }

      case Expression.FixedFunctionValue(_, _) => Some(assignments)

      case Expression.ConstructorValue(_, _, _) => Some(assignments)

      case expression@Expression.UntypedConstructorValue(binding, tpe, _) =>
        if (binding.isConstant) {
          InferenceVariable.assign(tpe, binding.underlyingType, assignments)
        } else {
          reporter.report(TypingFeedback.ConstructorValues.TypeContextExpected(expression))
          None
        }

      case Expression.ListConstruction(values, _) => infer(values, assignments)

      case Expression.MapConstruction(entries, _) =>
        infer(entries.map(_.key), assignments)
          .flatMap(infer(entries.map(_.value), _))

      case Expression.ShapeValue(properties, _) => infer(properties.map(_.value), assignments)

      case Expression.Symbol(_, _) => Some(assignments)

      case Expression.PropertyDefaultValue(_, _) => Some(assignments)

      case expression@Expression.UnaryOperation(operator, value, _, _) =>
        import UnaryOperator._
        def checkOperand(t1: Type) = checker.check(value, t1, assignments)

        operator match {
          case Negation => ArithmeticSynthesizer.infer(expression, Vector(value), assignments)
          case LogicalNot => checkOperand(BasicType.Boolean).flatMap(assignOperationResult(_, expression, BasicType.Boolean))
        }

      case expression@Expression.BinaryOperation(operator, left, right, _, _) =>
        import BinaryOperator._
        def checkOperands(t1: Type, t2: Type) = checker.check(left, t1, assignments).flatMap(checker.check(right, t2, _))

        operator match {
          case Addition | Subtraction | Multiplication | Division =>
            ArithmeticSynthesizer.infer(expression, Vector(left, right), assignments)

          case Equals | LessThan | LessThanEquals =>
            checkOperands(BasicType.Any, BasicType.Any)
              .flatMap(assignOperationResult(_, expression, BasicType.Boolean))

          case Append =>
            infer(left, assignments).flatMap { collectionAssignments =>
              val collectionType = InferenceVariable.instantiateCandidate(left, collectionAssignments)

              // The appended element's type might need to be informed by the collection's type, for example when
              // the collection is a list of functions and the appended element is an anonymous function without
              // parameter type annotations. Hence, we first attempt to check the appended element with the expected
              // element type. This is not always valid, though, because appending might widen the type of the list.
              // When checking fails, we thus need to default to inference.
              def checkAppendedElement(expectedElementType: Type): Option[(Assignments, Type)] = {
                val (checkedAssignments, _) = checker.attempt(right, expectedElementType, collectionAssignments)
                checkedAssignments.orElse(infer(right, collectionAssignments)).map {
                  elementAssignments => (elementAssignments, InferenceVariable.instantiateCandidate(right, elementAssignments))
                }
              }

              collectionType match {
                case ListType(elementType) =>
                  checkAppendedElement(elementType).flatMap { case (elementAssignments, appendedElementType) =>
                    val combinedType = ListType(SumType.construct(elementType, appendedElementType))
                    Unification.unifyEquals(expression.tpe, combinedType, elementAssignments)
                  }

                case _ =>
                  reporter.report(TypingFeedback.Lists.ListExpected(expression, collectionType))
                  None
              }
            }
        }

      case expression@Expression.XaryOperation(operator, operands, _, _) =>
        import XaryOperator._
        def checkOperands(tpe: Type) = checker.check(operands, tpe, assignments)

        operator match {
          case Conjunction | Disjunction => checkOperands(BasicType.Boolean).flatMap(assignOperationResult(_, expression, BasicType.Boolean))
          case Concatenation => checkOperands(BasicType.Any).flatMap(assignOperationResult(_, expression, BasicType.String))
        }

      case expression@Expression.Call(target, arguments, tpe, _) =>
        target match {
          case CallTarget.MultiFunction(mf) => MultiFunctionCallSynthesizer.infer(mf, expression, assignments)

          case CallTarget.Value(target) =>
            infer(target, assignments).flatMap { targetAssignments =>
              val argumentsResult = InferenceVariable.instantiateCandidate(target, targetAssignments) match {
                case FunctionType(input, output) =>
                  if (arguments.length == input.elements.length) {
                    val assignments3 = arguments.zip(input.elements).foldSome(targetAssignments) {
                      case (assignments2, (argument, parameterType)) =>
                        checker.check(argument, parameterType, assignments2)
                    }
                    assignments3.map((_, output))
                  } else {
                    reporter.error(TypingFeedback.ValueCalls.IllegalArity(expression, input))
                    None
                  }

                case targetType =>
                  reporter.error(TypingFeedback.ValueCalls.FunctionExpected(expression, targetType))
                  None
              }
              argumentsResult.flatMap {
                case (argumentAssignments, output) => Unification.unifyEquals(tpe, output, argumentAssignments)
              }
            }

          case CallTarget.Constructor(structBinding) => ConstructorCallSynthesizer.infer(structBinding, expression, assignments)

          case CallTarget.Intrinsic(_) => infer(arguments, assignments)
        }

      case Expression.Cond(cases, _) =>
        checker.check(cases.map(_.condition), BasicType.Boolean, assignments)
          .flatMap(infer(cases.map(_.body), _))

      case Expression.WhileLoop(condition, body, _) =>
        checker.check(condition, BasicType.Boolean, assignments)
          .flatMap(infer(body, _))

      case Expression.ForLoop(extractors, body, _) =>
        inferExtractors(extractors, assignments)
          .flatMap(infer(body, _))

      case Expression.Ascription(value, expectedType, _) => checker.check(value, expectedType, assignments)
    }

    resultAssignments.foreach(Typing.traceExpressionType(expression, _, "Inferred"))
    resultAssignments
  }

  /**
    * Unifies `resultType` with `operation.tpe` such that `operation.tpe` is a subtype of `resultType`.
    *
    * TODO (assembly): We can inline this function. The instantiation functionality isn't even used.
    *
    * @param resultType The result type will be instantiated by this function, so it is possible to pass an
    *                   uninstantiated type.
    */
  private def assignOperationResult(
    assignments: Assignments,
    operation: Expression,
    resultType: Type,
  )(implicit reporter: Reporter): Option[Assignments] = {
    Unification.unifyEquals(operation.tpe, InferenceVariable.instantiateCandidate(resultType, assignments), assignments)
  }

  def inferExtractors(extractors: Vector[Expression.Extractor], assignments: Assignments)(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    extractors.foldSome(assignments) {
      case (assignments2, extractor) =>
        infer(extractor.collection, assignments2).flatMap { assignments3 =>
          val collectionType = InferenceVariable.instantiateCandidate(extractor.collection, assignments3)
          val elementType = collectionType match {
            case ListType(element) => Some(element)
            case MapType(key, value) => Some(TupleType(key, value))
            case _ =>
              reporter.error(TypingFeedback.Loops.CollectionExpected(collectionType, extractor.collection))
              None
          }

          elementType.flatMap(Unification.unifyEquals(_, extractor.variable.tpe, assignments3))
        }
    }
  }

  /**
    * Executes [[infer]] for all `expressions` in order.
    */
  def infer(expressions: Vector[Expression], assignments: Assignments)(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    expressions.foldSome(assignments) {
      case (assignments2, expression) => infer(expression, assignments2)
    }
  }

  /**
    * Attempts type inference via [[infer]], using an internal reporter that accumulates errors, which are then
    * returned separately.
    *
    * `attempt` can be used to try a particular inference path without committing to it.
    */
  def attempt(expression: Expression, assignments: Assignments)(implicit checker: Checker): (Option[Assignments], Vector[Feedback]) = {
    implicit val reporter: MemoReporter = MemoReporter()
    (infer(expression, assignments), reporter.feedback)
  }

}
