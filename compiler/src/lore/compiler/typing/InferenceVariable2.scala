package lore.compiler.typing

import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.InferenceBounds.BoundType
import lore.compiler.inference.{InferenceBounds, InferenceVariable}
import lore.compiler.types.{BasicType, Type}

object InferenceVariable2 {

  /**
    * Returns the effective bounds of `iv`, which may or may not be contained in `assignments`.
    */
  def getBounds(iv: InferenceVariable, assignments: Assignments): InferenceBounds = {
    assignments.getOrElse(iv, InferenceBounds(iv, BasicType.Nothing, BasicType.Any))
  }

  /**
    * Assigns `lowerBound` to `iv`'s lower bound and `upperBound` to its upper bound. Returns None if the old and new
    * bounds are incompatible.
    */
  def assign(
    iv: InferenceVariable,
    lowerBound: Type,
    upperBound: Type,
    assignments: Assignments,
  ): Option[Assignments] = {
    assign(iv, lowerBound, BoundType.Lower, assignments)
      .flatMap(assign(iv, upperBound, BoundType.Upper, _))
  }

  /**
    * Assigns `bound` to `iv`'s lower and upper bound. Returns None if the old and new bounds are incompatible.
    */
  def assign(
    iv: InferenceVariable,
    bound: Type,
    assignments: Assignments,
  ): Option[Assignments] = {
    assign(iv, bound, bound, assignments)
  }

  /**
    * Assigns `bound` to the lower or upper bound of `iv`, depending on `boundType`. Returns None if the old and new
    * bounds are incompatible.
    *
    * TODO (inference): Sum/intersection type construction, like in the old inference model, or is this not needed here?
    */
  def assign(
    iv: InferenceVariable,
    bound: Type,
    boundType: BoundType,
    assignments: Assignments,
  ): Option[Assignments] = {
    val bounds = getBounds(iv, assignments)
    if (bounds.lower <= bound && bound <= bounds.upper) {
      val updatedBounds = boundType match {
        case BoundType.Lower => InferenceBounds(iv, bound, bounds.upper)
        case BoundType.Upper => InferenceBounds(iv, bounds.lower, bound)
      }
      Some(assignments.updated(iv, updatedBounds))
    } else None
  }

  /**
    * Ensures that `lowerBound` is a subtype of `iv`'s lower bound and `upperBound` is a supertype of `iv`'s upper
    * bound. If this is not the case, the respective bound will be assigned to `iv`.
    *
    * TODO (inference): Do we need this?
    */
  def ensure(
    iv: InferenceVariable,
    lowerBound: Type,
    upperBound: Type,
    assignments: Assignments,
  ): Option[Assignments] = {
    ensure(iv, lowerBound, BoundType.Lower, assignments)
      .flatMap(ensure(iv, upperBound, BoundType.Upper, _))
  }

  /**
    * Ensures that `bound` is a subtype/supertype of `iv`'s lower or upper bound, depending on `boundType`. If this is
    * not the case, `bound` will be assigned to `iv`.
    *
    * In practical terms, the function thus assures that a given subtyping relationship holds, either by validating it
    * directly or by changing the bounds to "make it fit".
    */
  def ensure(
    iv: InferenceVariable,
    bound: Type,
    boundType: BoundType,
    assignments: Assignments,
  ): Option[Assignments] = {
    val bounds = getBounds(iv, assignments)

    boundType match {
      case BoundType.Lower => if (bound <= bounds.lower) return Some(assignments)
      case BoundType.Upper => if (bounds.upper <= bound) return Some(assignments)
    }

    assign(iv, bound, boundType, assignments)
  }

}
