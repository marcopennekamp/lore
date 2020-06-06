package lore.types

class TypeVariable(val name: String, val lowerBound: Type, val upperBound: Type) extends NamedType {
  override val isAbstract: Boolean = false // TODO: Is this correct?
  override val isPolymorphic = true
  override def string(parentPrecedence: TypePrecedence): String = name

  // Type variables are strictly reference-equal.
  override def equals(obj: Any): Boolean = obj match {
    case var2: TypeVariable => this eq var2
    case _ => false
  }
  override def hashCode(): Int = name.hashCode
}
