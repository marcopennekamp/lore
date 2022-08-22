package lore.compiler.typing2.unification

import lore.compiler.types.{BasicType, Type}
import lore.compiler.typing2.unification.InferenceBounds2.BoundType2

case class InferenceBounds2(
  iv: InferenceVariable2,
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
  def get(boundType: BoundType2): Type = boundType match {
    case BoundType2.Lower => lower
    case BoundType2.Upper => upper
  }

  override def toString: String = s"$iv($lower, $upper)"
}

object InferenceBounds2 {
  sealed trait BoundType2

  object BoundType2 {
    case object Lower extends BoundType2
    case object Upper extends BoundType2

    def flip(boundType: BoundType2): BoundType2 = boundType match {
      case BoundType2.Lower => BoundType2.Upper
      case BoundType2.Upper => BoundType2.Lower
    }
  }
}
