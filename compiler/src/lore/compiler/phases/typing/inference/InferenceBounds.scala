package lore.compiler.phases.typing.inference

import lore.compiler.core.{Compilation, Error}
import lore.compiler.phases.typing.inference.Inference.Assignments
import lore.compiler.phases.typing.inference.InferenceVariable.effectiveBounds
import lore.compiler.types.{BasicType, Subtyping, Type}

// TODO: Do we still need the Option bounds now that Equals resolution has been fixed?
//          - It would seem so, as we now also need to differentiate between inference variables without any upper bound.

case class InferenceBounds(variable: InferenceVariable, lower: Option[Type], upper: Option[Type]) {
  val lowerOrNothing: Type = lower.getOrElse(BasicType.Nothing)
  val upperOrAny: Type = upper.getOrElse(BasicType.Any)

  /**
    * The candidate type is used as the effectively inferred type and thus the inference result. This will most likely
    * be the upper bound of the inference variable, but may also be the lower bound if the variable `iv` can only be
    * inferred from `t <= iv` Subtypes or least upper bound judgments.
    *
    * TODO: Return an option instead of Any?
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

  case class InvalidLowerBound(inferenceVariable: InferenceVariable, newLowerBound: Type, currentBounds: InferenceBounds, context: TypingJudgment) extends Error(context) {
    override def message: String = s"Type error: $newLowerBound must be a supertype of lower bound ${currentBounds.lowerOrNothing} and a subtype of upper bound ${currentBounds.upperOrAny}."
  }

  case class InvalidUpperBound(inferenceVariable: InferenceVariable, newUpperBound: Type, currentBounds: InferenceBounds, context: TypingJudgment) extends Error(context) {
    override def message: String = s"Type error: $newUpperBound must be a subtype of upper bound ${currentBounds.upperOrAny} and a supertype of lower bound ${currentBounds.lowerOrNothing}."
  }

  def narrowBounds(assignments: Assignments, inferenceVariable: InferenceVariable, lowerBound: Type, upperBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    narrowBound(assignments, inferenceVariable, lowerBound, BoundType.Lower, context).flatMap(
      narrowBound(_, inferenceVariable, upperBound, BoundType.Upper, context)
    )
  }

  /**
    * Attempts to narrow the lower/upper bound of the inference variable to the given new lower/upper bound. If the
    * variable already has a lower/upper bound, the new lower/upper bound must supertype/subtype the existing bound.
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
    val bounds = effectiveBounds(assignments, inferenceVariable)

    if (bounds.lower.forall(_ <= lowerBound) && lowerBound <= bounds.upperOrAny) {
      Compilation.succeed(assignments.updated(inferenceVariable, InferenceBounds(inferenceVariable, Some(lowerBound), bounds.upper)))
    } else {
      Compilation.fail(InvalidLowerBound(inferenceVariable, lowerBound, bounds, context))
    }
  }

  /**
    * Attempts to narrow the upper bound of the inference variable to the given new upper bound. If the variable
    * already has an upper bound, the new upper bound must subtype the existing bound.
    */
  def narrowUpperBound(assignments: Assignments, inferenceVariable: InferenceVariable, upperBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    val bounds = effectiveBounds(assignments, inferenceVariable)

    if (bounds.upper.forall(upperBound <= _) && bounds.lowerOrNothing <= upperBound) {
      Compilation.succeed(assignments.updated(inferenceVariable, InferenceBounds(inferenceVariable, bounds.lower, Some(upperBound))))
    } else {
      Compilation.fail(InvalidUpperBound(inferenceVariable, upperBound, bounds, context))
    }
  }

  /**
    * Override the bounds of the given inference variable without checking the previous bounds. This function is useful
    * when processing "dependent" typings that might change the bounds of an inference variable altogether based on
    * some other changing inference variables.
    */
  def overrideBounds(assignments: Assignments, inferenceVariable: InferenceVariable, lowerBound: Type, upperBound: Type): Assignments = {
    assignments.updated(inferenceVariable, InferenceBounds(inferenceVariable, Some(lowerBound), Some(upperBound)))
  }

  /**
    * @see [[ensureBoundSupertypes]], [[ensureBoundSubtypes]]
    */
  def ensureBound(assignments: Assignments, inferenceVariable: InferenceVariable, bound: Type, boundType: BoundType, context: TypingJudgment): Compilation[Assignments] = boundType match {
    case BoundType.Lower => ensureBoundSupertypes(assignments, inferenceVariable, bound, context)
    case BoundType.Upper => ensureBoundSubtypes(assignments, inferenceVariable, bound, context)
  }

  /**
    * Ensures that the inference variable's lower bound is a supertype of the given lower bound. If this is not the
    * case already, the function will attempt to narrow the inference variable's lower bound.
    *
    * In practical terms, the function thus assures that a given supertyping relationship holds, either by validating
    * it directly or by changing the bounds to "make it fit".
    */
  def ensureBoundSupertypes(assignments: Assignments, inferenceVariable: InferenceVariable, lowerBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    val bounds = effectiveBounds(assignments, inferenceVariable)

    println(s"$bounds ensure lower $lowerBound")

    if (Subtyping.isSubtype(lowerBound, bounds.lowerOrNothing)) {
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
    val bounds = effectiveBounds(assignments, inferenceVariable)

    println(s"$bounds ensure upper $upperBound")

    if (Subtyping.isSubtype(bounds.upperOrAny, upperBound)) {
      Compilation.succeed(assignments)
    } else {
      narrowUpperBound(assignments, inferenceVariable, upperBound, context)
    }
  }

}
