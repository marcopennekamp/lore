package lore.compiler.types

import lore.compiler.core.Scope

class TypeVariable(
  val name: String,
  val lowerBound: Type,
  val upperBound: Type,
  val declarationOrder: Int,
) extends NamedType with Scope.Entry {
  // Type variables are strictly reference-equal.
  override def equals(obj: Any): Boolean = obj match {
    case var2: TypeVariable => this eq var2
    case _ => false
  }
  override def hashCode(): Int = name.hashCode
}

object TypeVariable {
  type Assignments = Map[TypeVariable, Type]
}
