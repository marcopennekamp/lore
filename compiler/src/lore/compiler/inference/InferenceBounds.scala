package lore.compiler.inference

import lore.compiler.core.Compilation
import lore.compiler.feedback.TypingFeedback.NarrowBoundFailed
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.InferenceBounds.BoundType
import lore.compiler.inference.InferenceVariable.effectiveBounds
import lore.compiler.types.{BasicType, Type}

case class InferenceBounds(variable: InferenceVariable, lower: Type, upper: Type) {
  /**
    * The candidate type is used as the effectively inferred type and thus the inference result. This will most likely
    * be the upper bound of the inference variable, but may also be the lower bound if the variable's upper bound
    * cannot be inferred.
    */
  val candidateType: Type = {
    if (upper != BasicType.Any) upper
    else if (lower != BasicType.Nothing) lower
    else BasicType.Any
  }

  /**
    * Returns the bound with the given bound type.
    */
  def get(boundType: BoundType): Type = boundType match {
    case BoundType.Lower => lower
    case BoundType.Upper => upper
  }

  override def toString: String = s"$variable($lower, $upper)"
}

object InferenceBounds {

  // In Lore: type BoundType = #lower | #upper
  sealed trait BoundType
  object BoundType {
    case object Lower extends BoundType
    case object Upper extends BoundType
  }

  /**
    * Whether bounds are already fixed and cannot change further.
    */
  def areFixed(bounds: InferenceBounds): Boolean = bounds.lower == bounds.upper

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

    if (bounds.lower <= lowerBound && lowerBound <= bounds.upper) {
      Compilation.succeed(assignments.updated(inferenceVariable, InferenceBounds(inferenceVariable, lowerBound, bounds.upper)))
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

    if (bounds.lower <= upperBound && upperBound <= bounds.upper) {
      Compilation.succeed(assignments.updated(inferenceVariable, InferenceBounds(inferenceVariable, bounds.lower, upperBound)))
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

    if (assignments.isDefinedAt(inferenceVariable) && lowerBound <= bounds.lower) {
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

    if (assignments.isDefinedAt(inferenceVariable) && bounds.upper <= upperBound) {
      Compilation.succeed(assignments)
    } else {
      narrowUpperBound(assignments, inferenceVariable, upperBound, context)
    }
  }

}
