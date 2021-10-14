package lore.compiler.typing.synthesizer

import lore.compiler.feedback.{Feedback, MemoReporter, Reporter, TypingFeedback2}
import lore.compiler.inference.Inference
import lore.compiler.inference.Inference.Assignments
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, UnaryOperator, XaryOperator}
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.types._
import lore.compiler.typing.{Helpers, InferenceVariable2}
import lore.compiler.typing.checker.Checker
import lore.compiler.typing.unification.Unification
import lore.compiler.utils.CollectionExtensions.VectorExtension

// TODO (inference): We're using the old definition of Assignments here, which might be correct. However, we need to
//                   reevaluate whether we need lower and upper inference variable bounds, or if a direct assignment
//                   of types suffices. Type parameters with various bounds might complicate this, however.
object Synthesizer {

  /**
    * Infers the type of `expression` solely from the shape of the expression and the current inference variable
    * assignments, producing a new set of assignments with which the type of `expression` can be instantiated. If the
    * type cannot be inferred, one or more errors are reported and `None` is returned.
    *
    * TODO (inference): Maybe return an option so that we can return `None` instead of `assignments` when an error has
    * been found.
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

      case Expression.Block(expressions, _) =>
        checker.check(expressions.init, BasicType.Any, assignments)
          .flatMap(infer(expressions.last, _))

      case Expression.BindingAccess(_, _) => Some(assignments)

      case Expression.MemberAccess(_, _, _) =>
        // TODO (inference): What to do here? This might become relevant if we only produce unresolved member accesses
        //                   if the instance type isn't known during expression transformation. Otherwise, this case
        //                   can result in a CompilationException because typing should only have to deal with
        //                   unresolved member accesses.
        ???

      case expression@Expression.UnresolvedMemberAccess(instance, name, memberInferenceVariable, _) =>
        // TODO (inference): Alternatively, we can call `check(instance, %{ <name>: <tpe> }, assignments)` instead to
        //                   get the other direction, which infers the instance type from the member access. (A valid
        //                   direction in the typing judgments world.) We'll have to test this practically before
        //                   implementing that direction, however.
        infer(instance, assignments).flatMap { assignments2 =>
          val instanceType = Helpers.instantiate(instance, assignments2)
          val memberType = instanceType.member(name) match {
            case Some(member) => member.tpe
            case None =>
              reporter.report(TypingFeedback2.Members.NotFound(expression, instanceType))
              BasicType.Nothing
          }

          // We must assign the member's type to the inference variable, which may be part of types of other
          // expressions, regardless of whether the member type itself can be inferred.
          InferenceVariable2.assign(memberInferenceVariable, memberType, assignments2)
        }

      case Expression.Literal(_, _, _) => Some(assignments)

      case Expression.Tuple(values, _) => infer(values, assignments)

      case expression@Expression.AnonymousFunction(_, body, _) =>
        // To infer the type of an anonymous function, its parameters must be fully annotated.
        if (expression.isFullyAnnotated) {
          infer(body, assignments)
        } else {
          reporter.report(TypingFeedback2.AnonymousFunctions.TypeContextExpected(expression))
          None
        }

      case expression@Expression.MultiFunctionValue(mf, _, _) =>
        // We can infer a multi-function value without a function type context if the multi-function has a single,
        // monomorphic function.
        mf.functions match {
          case Vector(function) if function.isMonomorphic =>
            MultiFunctionValueSynthesizer.handleFunctionInstance(function.monomorphicInstance, expression, None, assignments)

          case _ =>
            reporter.error(TypingFeedback2.MultiFunctionValues.TypeContextExpected(expression))
            None
        }

      case Expression.FixedFunctionValue(_, _) => Some(assignments)

      case Expression.ConstructorValue(_, _, _) => Some(assignments)

      case expression@Expression.UntypedConstructorValue(binding, tpe, _) =>
        if (binding.isConstant) {
          InferenceVariable2.assign(tpe, binding.asSchema.representative, assignments)
        } else {
          reporter.report(TypingFeedback2.ConstructorValues.TypeContextExpected(expression))
          None
        }

      case Expression.ListConstruction(values, _) => infer(values, assignments)

      case Expression.MapConstruction(entries, _) =>
        infer(entries.map(_.key), assignments)
          .flatMap(infer(entries.map(_.value), _))

      case Expression.ShapeValue(properties, _) => infer(properties.map(_.value), assignments)

      case Expression.Symbol(_, _) => Some(assignments)

      case expression@Expression.UnaryOperation(operator, value, _, _) =>
        import UnaryOperator._
        def checkOperand(t1: Type) = checker.check(value, t1, assignments)

        operator match {
          case Negation => checkOperand(BasicType.Real).flatMap(assignOperationResult(_, expression, value.tpe, BasicType.Real))
          case LogicalNot => checkOperand(BasicType.Boolean).flatMap(assignOperationResult(_, expression, BasicType.Boolean))
        }

      case expression@Expression.BinaryOperation(operator, left, right, _, _) =>
        import BinaryOperator._
        def checkOperands(t1: Type, t2: Type) = checker.check(left, t1, assignments).flatMap(checker.check(right, t2, _))

        operator match {
          case Addition | Subtraction | Multiplication | Division =>
            checkOperands(BasicType.Real, BasicType.Real).flatMap(
              assignOperationResult(_, expression, SumType.construct(left.tpe, right.tpe), BasicType.Real)
            )

          case Equals | LessThan | LessThanEquals =>
            checkOperands(BasicType.Any, BasicType.Any)
              .flatMap(assignOperationResult(_, expression, BasicType.Boolean))

          case Append =>
            infer(left, assignments).flatMap(infer(right, _)).flatMap { assignments2 =>
              val collectionType = Helpers.instantiate(left, assignments2)
              val newElementType = Helpers.instantiate(right, assignments2)

              // We have to combine the collection's element type with the new element's type. This is only possible if
              // `collectionType` is actually a list.
              collectionType match {
                case ListType(elementType) =>
                  val combinedType = ListType(SumType.construct(elementType, newElementType))
                  Unification.unifyEquals(expression.tpe, combinedType, assignments2)

                case _ =>
                  reporter.report(TypingFeedback2.Lists.ListExpected(expression, collectionType))
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
          case CallTarget.Value(target) =>
            // TODO (inference): We can technically `check` the `target` here with the types of the arguments. This
            //                   complicates things, however, as most of the time we want to infer an argument's actual
            //                   type from the target's function type. So we'd first have to try to infer the
            //                   arguments, if everything can be inferred `check` the `target`, and if they cannot be
            //                   inferred, `infer` `target` instead.
            // TODO (inference): If we want to assign types to type parameters of constructors here, we'll have to
            //                   allow unresolved inference variables and somehow also implement their bounds. What's
            //                   probably easier is to treat constructor calls as a separate entity and handle them
            //                   like single-function multi-function calls. We will essentially need the same type
            //                   parameter handling for both multi-functions and constructors.

            infer(target, assignments).flatMap { targetAssignments =>
              val argumentsResult = Helpers.instantiate(target, targetAssignments) match {
                case FunctionType(input, output) =>
                  if (arguments.length == input.elements.length) {
                    val assignments3 = arguments.zip(input.elements).foldSome(targetAssignments) {
                      case (assignments2, (argument, parameterType)) =>
                        checker.check(argument, parameterType, assignments2)
                    }
                    assignments3.map((_, output))
                  } else {
                    reporter.error(TypingFeedback2.ValueCalls.IllegalArity(expression, input))
                    None
                  }

                case targetType =>
                  reporter.error(TypingFeedback2.ValueCalls.FunctionExpected(expression, targetType))
                  None
              }
              argumentsResult.flatMap {
                case (argumentAssignments, output) => Unification.unifyEquals(tpe, output, argumentAssignments)
              }
            }

          case CallTarget.MultiFunction(mf) => MultiFunctionCallSynthesizer.infer(mf, expression, assignments)

          case CallTarget.Constructor(structBinding) => ConstructorCallSynthesizer.infer(structBinding, expression, assignments)

          case CallTarget.Dynamic(_) => infer(arguments, assignments)
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

      // TODO (inference): This is missing type ascription, which delegates back to the checker! Obviously we'll have
      //                   to support this in the syntax first. (Such as `expr :: Type`.)
    }

    resultAssignments.foreach(Helpers.traceExpressionType(expression, _, "Inferred"))
    resultAssignments
  }

  /**
    * Unifies `resultType` with `operation.tpe` such that `operation.tpe` is a subtype of `resultType`, unless
    * `resultType` is not a subtype of `upperBound`, in which case `upperBound` is unified with `operationType`.
    *
    * @param resultType The result type will be instantiated by this function, so it is possible to pass an
    *                   uninstantiated type.
    * @param upperBound The upper bound ensures that the result type of an expression must adhere to certain standards.
    *                   For example, the result type of a negation is the type of its value, but if the value isn't Int
    *                   or Real, we don't want the negation to suddenly be typed as a String.
    */
  private def assignOperationResult(
    assignments: Assignments,
    operation: Expression,
    resultType: Type,
    upperBound: Type,
  )(implicit reporter: Reporter): Option[Assignments] = {
    val resultType2 = Helpers.instantiate(resultType, assignments, operation)
    val resultType3 = if (resultType2 <= upperBound) resultType2 else upperBound
    Unification.unifyEquals(operation.tpe, resultType3, assignments)
  }

  private def assignOperationResult(assignments: Assignments, operation: Expression, resultType: Type)(implicit reporter: Reporter): Option[Assignments] = {
    assignOperationResult(assignments, operation, resultType, resultType)
  }

  def inferExtractors(extractors: Vector[Expression.Extractor], assignments: Assignments)(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    extractors.foldSome(assignments) {
      case (assignments2, extractor) =>
        infer(extractor.collection, assignments2).flatMap { assignments3 =>
          val collectionType = Helpers.instantiate(extractor.collection, assignments3)
          val elementType = collectionType match {
            case ListType(element) => Some(element)
            case MapType(key, value) => Some(TupleType(key, value))
            case _ =>
              reporter.error(TypingFeedback2.Loops.CollectionExpected(collectionType, extractor.collection))
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
