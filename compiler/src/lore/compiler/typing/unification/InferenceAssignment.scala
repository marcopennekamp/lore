package lore.compiler.typing.unification

import lore.compiler.types.{BasicType, Type}
import lore.compiler.typing.unification.InferenceVariable.BoundType

case class InferenceAssignment(
  iv: InferenceVariable,
  lower: Type,
  upper: Type,
) {
  /**
    * The candidate type is the effectively inferred type for the inference variable. This will most likely be the
    * lower assignment (representing the narrowest possible type), but may also be the upper assignment if the
    * variable's lower assignment cannot be inferred.
    */
  val candidateType: Option[Type] = {
    if (lower != BasicType.Nothing) Some(lower)
    else if (upper != BasicType.Any) Some(upper)
    else None
  }

  /**
    * Returns the assigned type with the given bound type.
    */
  def get(boundType: BoundType): Type = boundType match {
    case BoundType.Lower => lower
    case BoundType.Upper => upper
  }

  override def toString: String = s"$iv($lower, $upper)"
}
