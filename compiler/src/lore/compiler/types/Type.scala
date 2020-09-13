package lore.compiler.types

import java.io.ByteArrayOutputStream
import java.util.Base64

import lore.compiler.utils.CollectionExtensions._

/**
  * Any Lore type.
  *
  * Note: hashCode should be defined as a val or lazy val to avoid recomputing hashes during the runtime of the
  * compiler. At times, we will hash types heavily, and so fast hash access is important.
  */
trait Type {
  // TODO: Add .product, .sum, and .intersection helper methods to List[Type] for easy construction.

  /**
    * Returns a singleton product type enclosing this type, unless this type is already a product type.
    */
  def toTuple: ProductType = ProductType(List(this))

  def <=(rhs: Type): Boolean = Subtyping.isSubtype(this, rhs)
  def <(rhs: Type): Boolean = Subtyping.isStrictSubtype(this, rhs)
  def >=(rhs: Type): Boolean = rhs <= this
  def >(rhs: Type): Boolean = rhs < this

  override def toString: String = Type.toString(this)
}

object Type {
  val predefinedTypes: Map[String, NamedType] = List(
    BasicType.Any,
    BasicType.Nothing,
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
      // Note that we consider the idea of augmenting trait types here. If the intersection type contains at least one
      // non-trait type, we ignore trait types in the consideration. If the intersection type consists only of traits,
      // it is NOT an augmented type and thus abstract.
      val exceptTraits = types.toList.filterNotType[TraitType]
      exceptTraits.isEmpty || exceptTraits.exists(isAbstract)
    case ProductType(elements) => elements.exists(isAbstract)
    case ListType(_) => false
    case MapType(_, _) => false
    case ComponentType(underlying) => isAbstract(underlying)
    case _: StructType => false
    case _: TraitType => true
    // Any isn't abstract because checking ARDS for it would be inadvisable.
    // Effectively, Nothing cannot be the supertype of anything, so declaring an abstract function for it
    // will only result in a useless deadlock.
    // All other basic types clearly aren't abstract either.
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
    case _: DeclaredType => false // TODO: For now. This needs to be set to true for structs/traits with type parameters, of course.
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
    case ComponentType(_) => Set.empty // TODO: Update when component types can have type parameters?
    case _: NamedType => Set.empty // TODO: Update when struct/trait types can have type parameters.
  }

  private sealed abstract class TypePrecedence(protected val value: Int) {
    def <(precedence: TypePrecedence): Boolean = this.value <= precedence.value
  }
  private object TypePrecedence {
    case object Parenthesized extends TypePrecedence(0)
    case object Sum extends TypePrecedence(1)
    case object Intersection extends TypePrecedence(2)
    case object Map extends TypePrecedence(3)
  }

  /**
    * Creates a pretty string representation of the type.
    */
  def toString(t: Type, verbose: Boolean = false): String = toStringWithPrecedence(t, verbose, TypePrecedence.Parenthesized)

  /**
    * Creates a pretty string representation of the type.
    */
  private def toStringWithPrecedence(t: Type, verbose: Boolean = false, parentPrecedence: TypePrecedence): String = {
    val infix = stringifyInfixOperator(parentPrecedence, toStringWithPrecedence(_, verbose, _)) _

    t match {
      case SumType(types) => infix(" | ", TypePrecedence.Sum, types.toList)
      case IntersectionType(types) => infix(" & ", TypePrecedence.Intersection, types.toList)
      case ProductType(elements) => s"(${elements.map(toString(_, verbose)).mkString(", ")})"
      case ListType(element) => s"[${toString(element, verbose)}]"
      case MapType(key, value) => infix(" -> ", TypePrecedence.Map, List(key, value))
      case ComponentType(underlying) => s"+${toString(underlying, verbose)}"
      case s: StructType =>
        if (verbose) {
          val implements = if (s.supertypes.nonEmpty) s" implements ${s.supertypes.map(toString(_, verbose)).mkString(", ")}" else ""
          s"struct ${s.name}$implements"
        } else s.name
      case t: TraitType =>
        if (verbose) {
          val extended = if (t.supertypes.nonEmpty) s" extends ${t.supertypes.map(toString(_, verbose)).mkString(", ")}" else ""
          s"label ${t.name}$extended"
        } else t.name
      case t: NamedType => t.name
    }
  }

  private def stringifyInfixOperator(
    parentPrecedence: TypePrecedence,
    stringify: (Type, TypePrecedence) => String,
  )(operator: String, operatorPrecedence: TypePrecedence, operands: List[Type]): String = {
    val repr = operands.map(stringify(_, operatorPrecedence)).mkString(operator)
    if (operatorPrecedence < parentPrecedence) s"($repr)" else repr
  }

  /**
    * Creates a unique, Javascript-friendly identifier of the given type.
    */
  def uniqueIdentifier(tpe: Type): String = uniqueIdentifier(List(tpe))

  /**
    * Creates a unique, Javascript-friendly identifier of the given list of types.
    *
    * The identifier is first generated as a compact binary representation of the given types individually,
    * concatenated to a single byte array, and then encoded using Base64 with '$' for the '+' character and
    * '_' for the '/' character. Padding characters are discarded as they are not needed for the identifier.
    */
  def uniqueIdentifier(types: List[Type]): String = {
    val stream = new ByteArrayOutputStream()
    types.foreach(t => stream.write(TypeEncoder.encode(t)))
    Base64.getEncoder.encodeToString(stream.toByteArray).replace('+', '$').replace('/', '_').replace("=", "")
  }
}
