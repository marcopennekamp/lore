package lore.compiler.types

import lore.compiler.core.UniqueKey
import lore.compiler.semantics.NamePath
import lore.compiler.types.TypeVariable.Variance

/**
  * Type variables are strictly equal based on their unique key.
  *
  * @param variance Variance is specified for all type variables, but only taken into account in traits and structs.
  *                 Variance has no bearing on type parameters of alias types and functions.
  * @param isOpen Openness is only taken into account in structs. It has no bearing on traits, alias types, and
  *               functions.
  * @param index The type variable's position in its type parameter list. This is used during poem type generation.
  */
case class TypeVariable(
  uniqueKey: UniqueKey,
  simpleName: String,
  lowerBound: Type,
  upperBound: Type,
  variance: Variance,
  isOpen: Boolean,
  index: Int,
) extends NamedType {
  override val name: NamePath = NamePath(simpleName)

  def withoutBounds: TypeVariable = this.copy(lowerBound = BasicType.Nothing, upperBound = BasicType.Any)

  override def equals(obj: Any): Boolean = obj match {
    case other: TypeVariable => this.uniqueKey == other.uniqueKey
    case _ => false
  }

  override def hashCode(): Int = uniqueKey.hashCode()
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
