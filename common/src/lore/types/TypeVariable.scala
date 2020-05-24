package lore.types

class TypeVariable(name: String, bound: Type) extends Type {
  override def isAbstract: Boolean = ???
  override def string(parentPrecedence: TypePrecedence): String = name

  // Type variables are strictly reference-equal.
  override def equals(obj: Any): Boolean = obj match {
    case var2: TypeVariable => this eq var2
    case _ => false
  }
  override def hashCode(): Int = name.hashCode
}
