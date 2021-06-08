package lore.compiler.phases.transformation.inference

import lore.compiler.core.Compilation
import lore.compiler.feedback.TypingFeedback.NarrowBoundFailed
import lore.compiler.phases.transformation.inference.Inference.Assignments
import lore.compiler.phases.transformation.inference.InferenceVariable.effectiveBounds
import lore.compiler.types.{BasicType, Subtyping, Type}

// TODO: Do we still need option bounds now that resolution order is graph-based?
//       I think this only hinges on whether we can implement member judgment resolution in such a way that we can
//       treat "instance Any" => "member Any" and "instance Nothing" => "member Nothing". Then we don't need to differentiate
//       between an instance that is inferred to be Any or Nothing vs. an instance that just doesn't have these bounds.
//       Typing Any.x as Any even though Any doesn't have a member x is technically not correct for the WHOLE language,
//       as we are disregarding that a valid member access must always access a member that exists in the first place.
//       But it might as well be defined as such for type inference only. Then we just have to ensure that an access to
//       a member that doesn't exist gets caught later!
//       The second use case for this is unification, specifically the function `unifyInferenceVariableWithType`. We
//       should find out whether this function can also be implemented without the Option bounds.

case class InferenceBounds(variable: InferenceVariable, lower: Option[Type], upper: Option[Type]) {
  val lowerOrNothing: Type = lower.getOrElse(BasicType.Nothing)
  val upperOrAny: Type = upper.getOrElse(BasicType.Any)

  /**
    * The candidate type is used as the effectively inferred type and thus the inference result. This will most likely
    * be the upper bound of the inference variable, but may also be the lower bound if the variable's upper bound
    * cannot be inferred.
    */
  val candidateType: Type = upper.orElse(lower).getOrElse(BasicType.Any)

  override def toString: String = s"$variable($lowerOrNothing, $upperOrAny)"
}

object InferenceBounds {

  // In Lore: type BoundType = #lower | #upper
  sealed trait BoundType
  object BoundType {
    case object Lower extends BoundType
    case object Upper extends BoundType
  }

  /**
    * Whether bounds may not change further.
    */
  def areFixed(bounds: InferenceBounds): Boolean = bounds.lower.exists(l => bounds.upper.contains(l))

  /**
    * Narrow the inference variable's lower bound to the given new lower bound and its upper bound to the given new
    * upper bound.
    */
  def narrowBounds(assignments: Assignments, inferenceVariable: InferenceVariable, lowerBound: Type, upperBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    narrowBound(assignments, inferenceVariable, upperBound, BoundType.Upper, context).flatMap(
      narrowBound(_, inferenceVariable, lowerBound, BoundType.Lower, context)
    )
  }

  /**
    * Narrow the inference variable's lower and upper bounds to the given new bound.
    */
  def narrowBounds(assignments: Assignments, inferenceVariable: InferenceVariable, bound: Type, context: TypingJudgment): Compilation[Assignments] = {
    narrowBounds(assignments, inferenceVariable, bound, bound, context)
  }

  /**
    * Attempts to narrow the lower/upper bound of the inference variable to the given new lower/upper bound.
    */
  def narrowBound(assignments: Assignments, inferenceVariable: InferenceVariable, bound: Type, boundType: BoundType, context: TypingJudgment): Compilation[Assignments] = boundType match {
    case BoundType.Lower => narrowLowerBound(assignments, inferenceVariable, bound, context)
    case BoundType.Upper => narrowUpperBound(assignments, inferenceVariable, bound, context)
  }

  /**
    * Attempts to narrow the lower bound of the inference variable to the given new lower bound. If the variable
    * already has a lower bound, the new lower bound must supertype the existing bound.
    */
  def narrowLowerBound(assignments: Assignments, inferenceVariable: InferenceVariable, lowerBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    val bounds = effectiveBounds(inferenceVariable, assignments)

    if (bounds.lower.forall(_ <= lowerBound) && lowerBound <= bounds.upperOrAny) {
      Compilation.succeed(assignments.updated(inferenceVariable, InferenceBounds(inferenceVariable, Some(lowerBound), bounds.upper)))
    } else {
      Compilation.fail(NarrowBoundFailed(inferenceVariable, lowerBound, BoundType.Lower, assignments, context))
    }
  }

  /**
    * Attempts to narrow the upper bound of the inference variable to the given new upper bound. If the variable
    * already has an upper bound, the new upper bound must subtype the existing bound.
    */
  def narrowUpperBound(assignments: Assignments, inferenceVariable: InferenceVariable, upperBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    val bounds = effectiveBounds(inferenceVariable, assignments)

    if (bounds.upper.forall(upperBound <= _) && bounds.lowerOrNothing <= upperBound) {
      Compilation.succeed(assignments.updated(inferenceVariable, InferenceBounds(inferenceVariable, bounds.lower, Some(upperBound))))
    } else {
      Compilation.fail(NarrowBoundFailed(inferenceVariable, upperBound, BoundType.Upper, assignments, context))
    }
  }

  /**
    * Ensures that the inference variable's lower bound is a supertype of the given lower bound. If this is not the
    * case already, the function will attempt to narrow the inference variable's lower bound.
    *
    * In practical terms, the function thus assures that a given supertyping relationship holds, either by validating
    * it directly or by changing the bounds to "make it fit".
    */
  def ensureBoundSupertypes(assignments: Assignments, inferenceVariable: InferenceVariable, lowerBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    val bounds = effectiveBounds(inferenceVariable, assignments)

    if (assignments.isDefinedAt(inferenceVariable) && Subtyping.isSubtype(lowerBound, bounds.lowerOrNothing)) {
      Compilation.succeed(assignments)
    } else {
      narrowLowerBound(assignments, inferenceVariable, lowerBound, context)
    }
  }

  /**
    * Ensures that the inference variable's upper bound is a subtype of the given upper bound. If this is not the
    * case already, the function will attempt to narrow the inference variable's upper bound.
    *
    * In practical terms, the function thus assures that a given subtyping relationship holds, either by validating
    * it directly or by changing the bounds to "make it fit".
    */
  def ensureBoundSubtypes(assignments: Assignments, inferenceVariable: InferenceVariable, upperBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    val bounds = effectiveBounds(inferenceVariable, assignments)

    if (assignments.isDefinedAt(inferenceVariable) && Subtyping.isSubtype(bounds.upperOrAny, upperBound)) {
      Compilation.succeed(assignments)
    } else {
      narrowUpperBound(assignments, inferenceVariable, upperBound, context)
    }
  }

}
