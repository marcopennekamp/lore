package lore.compiler.typing

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Reporter, TypingFeedback2}
import lore.compiler.inference.Inference.Assignments
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{BasicType, TupleType, Type}

// TODO (inference): We're using the old definition of Assignments here, which might be correct. However, we need to
//                   reevaluate whether we need lower and upper inference variable bounds, or if a direct assignment
//                   of types suffices. Type parameters with various bounds might complicate this, however.
object Synthesizer {

  /**
    * Infers the type of `expression` solely from the shape of the expression and the current inference variable
    * assignments, producing a new set of assignments with which the type of `expression` can be instantiated. If the
    * type cannot be inferred, one or more errors are reported.
    */
  def infer(expression: Expression, assignments: Assignments)(implicit checker: Checker, reporter: Reporter): Assignments = {
    // Delegates the handling of the expression to the Checker.
    def delegate(expectedType: Type): Assignments = checker.check(expression, expectedType, assignments)

    // Note that some expressions, such as `Return`, are handled by the Checker, so we need to delegate back to it.
    // Because the Synthesizer doesn't know about the expected type, this can only ever be a predetermined one. The
    // other case is returning a "static result", which happens when we try to infer leaf expressions such as literals
    // or binding accesses.
    // To avoid confusion between these two default cases, this match doesn't have a default case.
    expression match {
      case Expression.Hole(_, _) => assignments

      // These delegations of top-level expressions are necessary, because the inference for blocks, for example, uses
      // inference mode to type its last expression. This expression may well be a return, variable declaration, or
      // assignment.
      case Expression.Return(_, _) => delegate(BasicType.Nothing)
      case Expression.VariableDeclaration(_, _, _, _) => delegate(TupleType.UnitType)
      case Expression.Assignment(_, _, _) => delegate(TupleType.UnitType)

      case Expression.Block(expressions, _) =>
        val initAssignments = checker.check(expressions.init, BasicType.Any, assignments)
        infer(expressions.last, initAssignments)

      case Expression.BindingAccess(_, _) => assignments

      case Expression.MemberAccess(_, _, _) =>
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
        val assignments2 = infer(instance, assignments)
        val memberType = Helpers.instantiate(instance, assignments2).member(name) match {
          case Some(member) => member.tpe
          case None => BasicType.Nothing
        }

        // We must assign the member's type to the inference variable, which may be part of types of other
        // expressions, regardless of whether the member type itself can be inferred.
        Helpers.assign(memberInferenceVariable, memberType, assignments2)
          .getOrElse(assignments2)

      case Expression.Literal(_, _, _) => assignments

      case Expression.Tuple(values, _) => infer(values, assignments)

      case expression@Expression.AnonymousFunction(_, body, _) =>
        // To infer the type of an anonymous function, its parameters must be fully annotated.
        if (expression.isFullyAnnotated) {
          infer(body, assignments)
        } else {
          reporter.report(TypingFeedback2.AnonymousFunction.TypeContextExpected(expression))
          assignments
        }

      // TODO (inference): ConstructorValue.

      case Expression.MultiFunctionValue(_, _, position) =>
        // TODO (inference): Is this the right way to go about it? Of course the exception would have to be a reported
        //                   error instead, but the general gist is that we need checking mode to decide a how a
        //                   MultiFunctionValue is resolved.
        throw CompilationException(s"Cannot get multi-function value at $position without a proper type context.")

      case Expression.FixedFunctionValue(_, _) => assignments

      case Expression.ListConstruction(values, _) => infer(values, assignments)

      case Expression.MapConstruction(entries, _) =>
        val assignments2 = infer(entries.map(_.key), assignments)
        infer(entries.map(_.value), assignments2)

      // TODO (inference): This is missing type ascription, which delegates back to the checker! Obviously we'll have
      //                   to support this in the syntax first. (Such as `expr :: Type`.)
    }
  }

  /**
    * Executes [[infer]] for all `expressions` in order.
    */
  def infer(expressions: Vector[Expression], assignments: Assignments)(implicit checker: Checker, reporter: Reporter): Assignments = {
    expressions.foldLeft(assignments) {
      case (assignments2, expression) => infer(expression, assignments2)
    }
  }

  // TODO (inference): Implement the `tryInference` function which uses a nested reporter and returns None if the
  //                   reporter contains any errors.

}
