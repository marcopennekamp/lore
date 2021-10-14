package lore.compiler.typing

import lore.compiler.types.{BasicType, Type}
import lore.compiler.typing.InferenceBounds.BoundType

case class InferenceBounds(variable: InferenceVariable, lower: Type, upper: Type) {

  /**
    * The candidate type is used as the effectively inferred type and thus the inference result. This will most likely
    * be the lower bound of the inference variable (representing the narrowest possible type), but may also be the
    * upper bound if the variable's lower bound cannot be inferred.
    */
  val candidateType: Type = {
    if (lower != BasicType.Nothing) lower
    else if (upper != BasicType.Any) upper
    else BasicType.Any // TODO: Shouldn't this be Nothing since we're favoring the lower bound now?
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
