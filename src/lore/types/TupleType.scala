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

  override def toString = "(" + components.mkString(", ") + ")"
}
