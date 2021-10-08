package lore.compiler.typing.unification

import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.InferenceBounds.BoundType
import lore.compiler.types._

object Unification {

  /**
    * @see [[EqualityUnification.unify]]
    */
  def unifyEquals(t1: Type, t2: Type, assignments: Assignments): Option[Assignments] = {
    EqualityUnification.unify(t1, t2, Vector(BoundType.Lower, BoundType.Upper), assignments)
  }

  /**
    * @see [[SubtypingUnification.unify]]
    */
  def unifySubtypes(t1: Type, t2: Type, assignments: Assignments): Option[Assignments] = {
    SubtypingUnification.unify(t1, t2, assignments)
  }

}
