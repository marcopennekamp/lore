package lore.compiler.typing

import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.Assignments
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{BasicType, Type}

object Synthesizer {

  // TODO (inference): We're using the old definition of Assignments here, which might be correct. However, we need to
  //                   reevaluate whether we need lower and upper inference variable bounds, or if a direct assignment
  //                   of types suffices. Type parameters with various bounds might complicate this, however.
  case class Result(tpe: Type, assignments: Assignments)

  def infer(expression: Expression, assignments: Assignments)(implicit checker: Checker, reporter: Reporter): Result = {
    // Creates a synthesis result that contains the given type and the unchanged assignments.
    def typeResult(tpe: Type): Result = Result(tpe, assignments)

    expression match {
      case Expression.Block(expressions, _) =>
        val initAssignments = expressions.init.foldLeft(assignments) {
          case (assignments, expression) => checker.check(expression, BasicType.Any, assignments)
        }
        Synthesizer.infer(expressions.last, initAssignments)

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

      // TODO (inference): This is missing type ascription, which delegates back to the checker! Obviously we'll have
      //                   to support this in the syntax first. (Such as `expr :: Type`.)

      // The general case of inference simply instantiates the expression's current type with the given assignments.
      // TODO (inference): This can only hold for childless expressions. If we have a `return`, for example, whose
      //                   subexpression must be checked, this cannot be covered by the general case. An exception
      //                   would be if this check is implemented in Checker, and the general case actually delegates to
      //                   Checker... But this conflicts with how bidirectional typing systems are usually (seemingly
      //                   to me) laid out: `check` delegates to `infer`, but `infer` doesn't delegate to `check`.
      case _ => typeResult(Helpers.instantiate(expression.tpe, assignments, expression))
    }
  }

}
