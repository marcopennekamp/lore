package lore.compiler.types

/**
  * Please note that type variables are strictly reference-equal. If you create two type variable objects with the
  * same names and bounds, they will NOT be equal!
  */
class TypeVariable(
  val name: String,
  val lowerBound: Type,
  val upperBound: Type,
  val declarationOrder: Int,
) extends NamedType {
  override def equals(obj: Any): Boolean = obj match {
    case var2: TypeVariable => this eq var2
    case _ => false
  }
}

object TypeVariable {
  type Assignments = Map[TypeVariable, Type]

  sealed trait Variance
  object Variance {
    case object Invariant extends Variance
    case object Covariant extends Variance
    case object Contravariant extends Variance
  }
}
