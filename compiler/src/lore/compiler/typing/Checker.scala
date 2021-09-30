package lore.compiler.typing

import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.inference.Inference.Assignments
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.Type

/**
  * @param returnType The return type of the surrounding function, used to check `Return` expressions.
  */
case class Checker(returnType: Type) {

  private implicit val checker: Checker = this

  def check(expression: Expression, expectedType: Type, assignments: Assignments)(implicit reporter: Reporter): Assignments = {
    // Step 1: Check and/or infer the expression's sub-expressions to produce an assignments map that will allow us to
    //         instantiate `expression.tpe`.
    val postAssignments = expression match {
      case Expression.Hole(_, _) => assignments // TODO (inference): Should we just ignore holes?

      case Expression.Return(value, _) =>
        check(value, returnType, assignments)

      case Expression.VariableDeclaration(_, value, typeAnnotation, _) =>
        // If a type annotation exists, we just have to check that the value has a compatible type. Otherwise, we need
        // to infer the type of the variable from the value expression.
        typeAnnotation match {
          case Some(typeAnnotation) => check(value, typeAnnotation, assignments)
          case None => Synthesizer.infer(value, assignments).assignments
        }

      case Expression.Assignment(target, value, _) =>
        val Synthesizer.Result(targetType, assignments2) = Synthesizer.infer(target, assignments)
        check(value, targetType, assignments2)

      // The general case delegates to the Synthesizer, which simply infers the type of the expression. This
      // corresponds to a particular rule in most bidirectional type systems, defined as such:
      //    If `Γ ⊢ e => A` (infer) and `A = B` then `Γ ⊢ e <= B` (checked)
      // See: "Jana Dunfield and Neel Krishnaswami. 2020. Bidirectional Typing."
      // Of course, in Lore's type system, type equality must be replaced with subtyping, i.e. `A subtypes B`.
      case _ => Synthesizer.infer(expression, assignments).assignments
    }

    // Step 2: Use the new assignments map to check that `expression.tpe` (as instantiated) is a subtype of
    //         `expectedType`.
    val actualType = Helpers.instantiate(expression.tpe, postAssignments, expression)
    if (actualType </= expectedType) {
      // TODO (inference): This probably needs a new typing error, maybe even dependent on the expression kind.
      reporter.error(TypingFeedback.SubtypeExpected(actualType, expectedType, expression))
    }
    postAssignments
  }

}
