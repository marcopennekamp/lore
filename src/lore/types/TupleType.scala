package lore.types

import scalaz.std.list._
import scalaz.syntax.traverse._

import lore.execution.Context

case class TupleType(components: Seq[Type]) extends Type {
  // TODO: Does it make sense that we are using the component type itself if it has no direct declared subtypes?
  /**
    * @return Direct declared subtypes for tuples are a bit special. There are two cases to handle:
    *           1. A component type has no direct declared subtypes. This means that for the purposes of this
    *              method, we will assume the type has no subtypes. We will instead take the component type
    *              as is, so tuple types are still generated.
    *           2. A component type has direct declared subtypes. This is the normal case in which we take
    *              the set of direct declared subtypes to build all combinatorially feasible tuple types.
    */
  override def directDeclaredSubtypes(implicit context: Context) = {
    components
      .map(t => if (t.directDeclaredSubtypes.isEmpty) List(t) else t.directDeclaredSubtypes.toList)
      .toList.sequence
      .map(types => TupleType(types))
      .toSet
  }

  /**
    * Since this is already a tuple, there is no need to enclose it in another tuple.
    */
  override def toTuple = this

  /**
    * @return Whether the tuple type is abstract. A tuple type is abstract if one of its component types is abstract.
    */
  override def isAbstract = components.exists(_.isAbstract)

  override def toString = "(" + components.mkString(", ") + ")"
}
