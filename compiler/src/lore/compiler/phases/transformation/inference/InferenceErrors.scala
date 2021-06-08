package lore.compiler.phases.transformation.inference

import lore.compiler.core.{CompilationException, Error, Positioned}
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiateByBound}
import lore.compiler.phases.transformation.inference.InferenceBounds.BoundType
import lore.compiler.types.Type

object InferenceErrors {

  case class EqualTypesExpected(t1: Type, t2: Type, context: TypingJudgment) extends Error(context) {
    override def message: String = s"The types $t1 and $t2 must be equal."
  }

  case class SubtypeExpected(actualType: Type, expectedType: Type, context: Positioned) extends Error(context) {
    override def message: String = s"This expression has the illegal type $actualType. We expected the following type (or a subtype thereof): $expectedType."
  }

  case class SupertypeExpected(actualType: Type, expectedType: Type, context: Positioned) extends Error(context) {
    override def message: String = s"This expression has the illegal type $actualType. We expected the following type (or a supertype thereof): $expectedType."
  }

  /**
    * Signifies that the lower or upper bound of the given inference variable cannot be narrowed to the given new
    * bound. The error message shown to the user depends on the given context. A Subtypes judgment will be reported
    * differently than a MemberAccess judgment, for example. The assignments are preserved so that all types can be
    * properly instantiated for reporting.
    */
  case class NarrowBoundFailed(iv: InferenceVariable, newBound: Type, boundType: BoundType, assignments: Assignments, context: TypingJudgment) extends Error(context) {
    override lazy val message: String = {
      val prepare = t => instantiateByBound(assignments, t, boundType)
      val relationshipName = boundType match {
        case BoundType.Lower => "supertype"
        case BoundType.Upper => "subtype"
      }
      context match {
        case TypingJudgment.Equals(t1, t2, _) => EqualTypesExpected(prepare(t1), prepare(t2), context).message
        case TypingJudgment.Subtypes(t1, t2, _) =>
          // We shall assume that the type containing `iv` is the "point of error", while the other type is the "source
          // of truth". This allows us to generate the correct user-facing error. For example, if we have a typing
          // judgment `x <:< B`, `B` is clearly the correct type, while `x` has been typed incorrectly. Thus we report
          // to the user: "`x` must be `B` or a subtype thereof".
          if (Inference.variables(t1).contains(iv)) {
            SubtypeExpected(prepare(t1), prepare(t2), context).message
          } else {
            SupertypeExpected(prepare(t2), prepare(t1), context).message
          }
        case TypingJudgment.Assign(target, source, _) => s"${prepare(source)} must be assignable to ${prepare(target)}."
        case TypingJudgment.Fits(t1, t2, _) => s"${prepare(t1)} must fit into ${prepare(t2)}."
        case TypingJudgment.LeastUpperBound(target, _, _) =>
          if (iv != target) {
            throw CompilationException(s"The backwards least upper bound typing judgment resolver should not have invoked narrowBounds. Context: $context.")
          }
          s"The least upper bound ${prepare(newBound)} must be a $relationshipName of ${prepare(iv)}."
        case TypingJudgment.MemberAccess(target, _, name, _) =>
          if (iv == target) {
            s"The type ${prepare(newBound)} of member $name must be a $relationshipName of ${prepare(iv)}."
          } else {
            s"The type ${prepare(iv)} cannot be narrowed to ${prepare(newBound)} and thus can't contain an appropriately typed member $name."
          }
        case TypingJudgment.ElementType(_, _, _) => s"The element type ${prepare(newBound)} must be a $relationshipName of ${prepare(iv)}."
        case TypingJudgment.MultiFunctionCall(_, mf, _, _) => s"The result type ${prepare(newBound)} of calling ${mf.name} must be a $relationshipName of ${prepare(iv)}."
        case TypingJudgment.MultiFunctionValue(_, _, _) => s"The function type ${prepare(newBound)} must be a $relationshipName of ${prepare(iv)}."
      }
    }
  }

}
