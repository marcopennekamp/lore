package lore.types

case class TupleType(components: Seq[Type]) extends Type {
  override def isSubtype(other: Type): Boolean = {
    other match {
      case TupleType(otherComponents) =>
        otherComponents.size == components.size && components.zip(otherComponents).forall {
          case (tpe, otherType) => tpe.isSubtype(otherType)
        }
      case IntersectionType(types) => types.exists(tpe => this.isSubtype(tpe))
      case _ => false
    }
  }

  /**
    * @return Whether the tuple type is abstract. A tuple type is abstract if one of its component types is abstract.
    */
  override def isAbstract = components.exists(_.isAbstract)

  override def toString = "(" + components.mkString(", ") + ")"
}
