package lore.compiler.types

import lore.compiler.semantics.NamePath
import lore.compiler.types.TypeVariable.Variance

/**
  * Type variables are strictly reference-equal. If you create two type variable objects with the same names and
  * bounds, they will NOT be equal.
  *
  * @param variance Variance is specified for all type variables, but only taken into account in traits and structs.
  *                 Variance has no bearing on type parameters of alias types and functions.
  * @param isOpen Openness is only taken into account in structs. It has no bearing on traits, alias types, and
  *               functions.
  * @param index The type variable's position in its type parameter list. This is used during poem type generation.
  */
class TypeVariable(
  val simpleName: String,
  val lowerBound: Type,
  val upperBound: Type,
  val variance: Variance,
  val isOpen: Boolean,
  val index: Int,
) extends NamedType {
  override val name: NamePath = NamePath(simpleName)

  override def equals(obj: Any): Boolean = obj match {
    case var2: TypeVariable => this eq var2
    case _ => false
  }
}

object TypeVariable {
  type Assignments = Map[TypeVariable, Type]

  sealed trait Variance {
    lazy val humanReadable: String = this.toString.toLowerCase
  }

  object Variance {
    case object Covariant extends Variance
    case object Contravariant extends Variance
    case object Invariant extends Variance

    def flip(variance: Variance): Variance = variance match {
      case Covariant => Contravariant
      case Contravariant => Covariant
      case Invariant => Invariant
    }
  }
}
