package lore.compiler.types

import lore.compiler.utils.CollectionExtensions._

/**
  * Any Lore type.
  *
  * Note: hashCode should be defined as a val or lazy val to avoid recomputing hashes during the runtime of the
  * compiler. At times, we will hash types heavily, and so fast hash access is important.
  */
trait Type {
  // TODO: As a general performance improvement, we might consider interning types like strings are interned,
  //       especially in the runtime.
  // TODO: Add .product, .sum, and .intersection helper methods to List[Type] for easy construction.

  /**
    * Returns a singleton product type enclosing this type, unless this type is already a product type.
    */
  def toTuple: ProductType = ProductType(List(this))

  /**
    * A pretty string representation of the type.
    */
  def string(parentPrecedence: TypePrecedence): String
  override def toString: String = string(TypePrecedence.Parenthesized)
  // TODO: Maybe we should move this definition to a TypePrinter object, as it's not a core part of a type and
  //       rather distracts from the important stuff. It's also annoying to define.

  def <=(rhs: Type): Boolean = Subtyping.isSubtype(this, rhs)
  def <(rhs: Type): Boolean = Subtyping.isStrictSubtype(this, rhs)
  def >=(rhs: Type): Boolean = rhs <= this
  def >(rhs: Type): Boolean = rhs < this
}

object Type {
  val predefinedTypes: Map[String, NamedType] = List(
    AnyType,
    NothingType,
    BasicType.Int,
    BasicType.Real,
    BasicType.Boolean,
    BasicType.String,
  ).map(t => (t.name, t)).toMap

  /**
    * Whether the given type is abstract.
    *
    * For an elaboration on the choices made here consult the specification.
    */
  def isAbstract(t: Type): Boolean = t match {
    case _: TypeVariable => false // TODO: Is this correct?
    case SumType(_) => true
    case IntersectionType(types) =>
      // Note that we consider the idea of augmenting label types here. If the intersection type contains at least one
      // non-label type, we ignore label types in the consideration.
      val exceptLabels = types.toList.filterNotType[LabelType]

      // If the intersection type consists only of labels, it is NOT an augmented type and thus abstract since
      // non-augmenting label types are abstract.
      if (exceptLabels.isEmpty) {
        true
      } else {
        // In all other cases, we decide abstractness WITHOUT taking augmenting labels into account.
        exceptLabels.exists(isAbstract)
      }
    case ProductType(elements) => elements.exists(isAbstract)
    case ListType(_) => false
    case MapType(_, _) => false
    case ComponentType(underlying) => isAbstract(underlying)
    case c: ClassType => c.isAbstract
    case _: LabelType =>
      // A label type is abstract unless it is an augmentation. That case is handled in the implementation of
      // intersection type's isAbstract.
      true
    case AnyType => false // Any isn't abstract because checking ARDS for it would be inadvisable.
    case NothingType =>
      // Effectively, Nothing cannot be the supertype of anything, so declaring an abstract function for it
      // will only result in a useless deadlock.
      false
    case _: BasicType => false
  }

  /**
    * Whether the given type contains a type variable.
    */
  def isPolymorphic(t: Type): Boolean = t match {
    case _: TypeVariable => true
    case SumType(types) => types.exists(isPolymorphic)
    case IntersectionType(types) => types.exists(isPolymorphic)
    case ProductType(elements) => elements.exists(isPolymorphic)
    case ListType(element) => isPolymorphic(element)
    case MapType(key, value) => isPolymorphic(key) || isPolymorphic(value)
    case ComponentType(_) => false // TODO: This might change once component types can be parameterized. (If they ever can.)
    case _: DeclaredType => false // TODO: For now. This needs to be set to true for classes with type parameters, of course.
    case _ => false
  }

  /**
    * Whether the given type contains no type variables and thus represents a single type instance.
    */
  def isMonomorphic(t: Type): Boolean = !isPolymorphic(t)

  /**
    * Returns all type variables that occur in the given type.
    */
  def variables(t: Type): Set[TypeVariable] = t match {
    case tv: TypeVariable => Set(tv)
    case SumType(types) => types.flatMap(variables)
    case IntersectionType(types) => types.flatMap(variables)
    case ProductType(components) => components.flatMap(variables).toSet
    case ListType(element) => variables(element)
    case MapType(key, value) => variables(key) ++ variables(value)
    case _: ComponentType => Set.empty // TODO: Update when component types can have type parameters?
    case _: NamedType => Set.empty // TODO: Update when class types can have type parameters.
  }
}

sealed abstract class TypePrecedence(protected val value: Int) {
  def <(precedence: TypePrecedence): Boolean = this.value <= precedence.value
}
object TypePrecedence {
  case object Parenthesized extends TypePrecedence(0)
  case object Sum extends TypePrecedence(1)
  case object Intersection extends TypePrecedence(2)
  case object Map extends TypePrecedence(3)
  case object Atom extends TypePrecedence(4)
}

trait OperatorType { self: Type =>
  override def string(parentPrecedence: TypePrecedence): String = {
    val repr = operands.map(_.string(precedence)).mkString(s" $operator ")
    if (precedence < parentPrecedence) s"($repr)" else repr
  }

  protected def precedence: TypePrecedence
  protected def operands: List[Type]
  protected def operator: String
}
