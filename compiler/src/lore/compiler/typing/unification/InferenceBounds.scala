package lore.compiler.typing.unification

import lore.compiler.types.{BasicType, Type}
import lore.compiler.typing.unification.InferenceBounds.BoundType

// TODO (multi-import): Rename InferenceBounds to InferenceAssignment. Rename lower to lowerBound and upper to
//                      upperBound. The reason for this is that bounds were already added to inference variables, and
//                      the term "inference bounds" conflicts with that.
case class InferenceBounds(
  iv: InferenceVariable,
  lower: Type,
  upper: Type,
) {
  /**
    * The candidate type is the effectively inferred type for the inference variable. This will most likely be the
    * lower bound of the inference variable (representing the narrowest possible type), but may also be the upper bound
    * if the variable's lower bound cannot be inferred.
    */
  val candidateType: Option[Type] = {
    if (lower != BasicType.Nothing) Some(lower)
    else if (upper != BasicType.Any) Some(upper)
    else None
  }

  /**
    * Returns the bound with the given bound type.
    */
  def get(boundType: BoundType): Type = boundType match {
    case BoundType.Lower => lower
    case BoundType.Upper => upper
  }

  override def toString: String = s"$iv($lower, $upper)"
}

object InferenceBounds {
  sealed trait BoundType

  object BoundType {
    case object Lower extends BoundType
    case object Upper extends BoundType

    def flip(boundType: BoundType): BoundType = boundType match {
      case BoundType.Lower => BoundType.Upper
      case BoundType.Upper => BoundType.Lower
    }
  }
}
