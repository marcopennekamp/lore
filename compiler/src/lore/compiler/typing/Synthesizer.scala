package lore.compiler.typing

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Reporter, TypingFeedback2}
import lore.compiler.inference.Inference.Assignments
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{BasicType, TupleType, Type}

object Synthesizer {

  // TODO (inference): We're using the old definition of Assignments here, which might be correct. However, we need to
  //                   reevaluate whether we need lower and upper inference variable bounds, or if a direct assignment
  //                   of types suffices. Type parameters with various bounds might complicate this, however.
  case class Result(tpe: Type, assignments: Assignments)

  def infer(expression: Expression, assignments: Assignments)(implicit checker: Checker, reporter: Reporter): Result = {
    // A result that contains the expression's type in instantiated form, as well as unchanged assignments.
    lazy val staticResult: Result = Result(Helpers.instantiate(expression.tpe, assignments, expression), assignments)

    // Creates a result with a new assignments map, using it to instantiate the type from the expression.
    def assignmentsResult(assignments2: Assignments): Result = Result(Helpers.instantiate(expression.tpe, assignments2, expression), assignments2)

    // Delegates the handling of the expression to the Checker.
    def delegate(expectedType: Type): Result = assignmentsResult(checker.check(expression, expectedType, assignments))

    // Note that some expressions, such as `Return`, are handled by the Checker, so we need to delegate back to it.
    // Because the Synthesizer doesn't know about the expected type, this can only ever be a predetermined one. The
    // other case is returning a "static result", which happens when we try to infer leaf expressions such as literals
    // or binding accesses.
    // To avoid confusion between these two default cases, this match doesn't have a default case.
    expression match {
      case Expression.Hole(_, _) => staticResult

      // These delegations of top-level expressions are necessary, because the inference for blocks, for example, uses
      // inference mode to type its last expression. This expression may well be a return, variable declaration, or
      // assignment.
      case Expression.Return(_, _) => delegate(BasicType.Nothing)
      case Expression.VariableDeclaration(_, _, _, _) => delegate(TupleType.UnitType)
      case Expression.Assignment(_, _, _) => delegate(TupleType.UnitType)

      case Expression.Block(expressions, _) =>
        val initAssignments = expressions.init.foldLeft(assignments) {
          case (assignments2, expression) =>
            // TODO (inference): Invoking the checker with expected type Any seems weird.
            checker.check(expression, BasicType.Any, assignments2)
        }
        infer(expressions.last, initAssignments)

      case Expression.BindingAccess(_, _) => staticResult

      case Expression.MemberAccess(instance, member, _) =>
        // TODO (inference): What to do here? This might become relevant if we only produce unresolved member accesses
        //                   if the instance type isn't known during expression transformation. Otherwise, this case
        //                   can result in a CompilationException because typing should only have to deal with
        //                   unresolved member accesses.
        ???

      case Expression.UnresolvedMemberAccess(instance, name, memberInferenceVariable, _) =>
        // TODO (inference): Alternatively, we can call `check(instance, %{ <name>: <tpe> }, assignments)` instead to
        //                   get the other direction, which infers the instance type from the member access. (A valid
        //                   direction in the typing judgments world.) We'll have to test this practically before
        //                   implementing that direction, however.
        val Result(instanceType, assignments2) = infer(instance, assignments)
        val memberType = instanceType.member(name) match {
          case Some(member) => member.tpe
          case None => BasicType.Nothing
        }

        // We must assign the member's type to the inference variable, which may be part of types of other
        // expressions, regardless of whether the member type itself can be inferred.
        Helpers.assign(memberInferenceVariable, memberType, assignments2)
          .map(Result(memberType, _))
          .getOrElse(Result(BasicType.Nothing, assignments2))

      case Expression.Literal(_, _, _) => staticResult

      case Expression.Tuple(values, _) =>
        val valueAssignments = values.foldLeft(assignments) {
          case (assignments2, value) =>  infer(value, assignments2).assignments
        }
        assignmentsResult(valueAssignments)

      case expression@Expression.AnonymousFunction(_, body, _) =>
        // To infer the type of an anonymous function, its parameters must be fully annotated.
        if (expression.isFullyAnnotated) {
          infer(body, assignments)
        } else {
          reporter.report(TypingFeedback2.AnonymousFunction.TypeContextExpected(expression))
          staticResult
        }

      case Expression.MultiFunctionValue(_, _, position) =>
        // TODO (inference): Is this the right way to go about it? Of course the exception would have to be a reported
        //                   error instead, but the general gist is that we need checking mode to decide a how a
        //                   MultiFunctionValue is resolved.
        throw CompilationException(s"Cannot get multi-function value at $position without a proper type context.")

      // TODO (inference): This is missing type ascription, which delegates back to the checker! Obviously we'll have
      //                   to support this in the syntax first. (Such as `expr :: Type`.)
    }
  }

  // TODO (inference): Implement the `tryInference` function which uses a nested reporter and returns None if the
  //                   reporter contains any errors.

}
