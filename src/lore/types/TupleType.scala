package lore.types

import scalaz.std.list._
import scalaz.syntax.traverse._

import lore.execution.Context

case class TupleType(components: Seq[Type]) extends Type {
  /**
    * The set of direct declared subtypes for a tuple type consists of the combinatorially constructed
    * direct declared subtypes of each component type.
    */
  override def directDeclaredSubtypes(implicit context: Context) = {
    Subtyping.directDeclaredSubtypeCombinations(components.toList).map(TupleType(_))
  }

  def abstractDirectDeclaredSubtypes(implicit context: Context): Set[TupleType] = {
    Subtyping.abstractDirectDeclaredSubtypeCombinations(components.toList).map(TupleType(_))
  }

  /**
    * Since this is already a tuple, there is no need to enclose it in another tuple.
    */
  override def toTuple = this

  /**
    * Whether the tuple type is abstract. A tuple type is abstract if one of its component types is abstract.
    */
  override def isAbstract = components.exists(_.isAbstract)

  override def toString = "(" + components.mkString(", ") + ")"
}
