package lore.compiler.types

import lore.compiler.semantics.Registry
import lore.compiler.types.TypeVariable.Assignments
import lore.compiler.utils.CollectionExtensions._
import scalaz.std.vector._
import scalaz.syntax.traverse._

import java.io.ByteArrayOutputStream
import java.util.Base64

/**
  * Any Lore type.
  *
  * Note: hashCode should be defined as a val or lazy val to avoid recomputing hashes during the runtime of the
  * compiler. At times, we will hash types heavily, and so fast hash access is important.
  */
trait Type extends HasMembers {
  def <=(rhs: Type): Boolean = Subtyping.isSubtype(this, rhs)
  def </=(rhs: Type): Boolean = !(this <= rhs)
  def <(rhs: Type): Boolean = Subtyping.isStrictSubtype(this, rhs)
  def >=(rhs: Type): Boolean = rhs <= this
  def >(rhs: Type): Boolean = rhs < this

  override def toString: String = Type.toString(this)
}

object Type {

  val predefinedTypes: Map[String, NamedType] = Vector(
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
    case _: TypeVariable =>
      // There may be cases in which a type variable can only represent abstract types, for example when a type
      // variable has traits as upper and lower bounds. Calling a function having such a type variable would be
      // effectively meaningless, however, since no value could inhabit the type expected by the type variable.
      // Additionally, it is not clear to me how a function with an abstract type variable would be intuitively
      // specialized. Hence, we decided that type variables must always be concrete.
      false
    case SumType(_) => true
    case IntersectionType(types) =>
      // Note that we consider the idea of augmenting trait types here. If the intersection type contains at least one
      // non-trait type, we ignore trait types in the consideration. If the intersection type consists only of traits,
      // it is NOT an augmented type and thus abstract.
      val exceptTraits = types.toVector.filterNotType[TraitType]
      exceptTraits.isEmpty || exceptTraits.exists(isAbstract)
    case TupleType(elements) => elements.exists(isAbstract)
    case _: TraitType => true
    case _: BasicType =>
      // Any isn't abstract because declaring an abstract function over it is more than inadvisable. Effectively,
      // Nothing cannot be the supertype of anything, so declaring an abstract function over it will only result in
      // dead code. All other basic types clearly aren't abstract either.
      false
    case _ => false
  }

  /**
    * Whether the given type is concrete.
    */
  def isConcrete(t: Type): Boolean = !isAbstract(t)

  /**
    * Whether the given type contains a type variable.
    */
  def isPolymorphic(t: Type): Boolean = t match {
    case _: TypeVariable => true
    case SumType(types) => types.exists(isPolymorphic)
    case IntersectionType(types) => types.exists(isPolymorphic)
    case TupleType(elements) => elements.exists(isPolymorphic)
    case FunctionType(input, output) => isPolymorphic(input) || isPolymorphic(output)
    case ListType(element) => isPolymorphic(element)
    case MapType(key, value) => isPolymorphic(key) || isPolymorphic(value)
    case ShapeType(properties) => properties.values.map(_.tpe).exists(isPolymorphic)
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
    case TupleType(elements) => elements.flatMap(variables).toSet
    case FunctionType(input, output) => variables(input) ++ variables(output)
    case ListType(element) => variables(element)
    case MapType(key, value) => variables(key) ++ variables(value)
    case ShapeType(properties) => properties.values.map(_.tpe).flatMap(variables).toSet
    case _ => Set.empty
  }

  /**
    * Substitute occurrences of variables in the given type with values from the given assignments. Variables that
    * do not occur in the assignments are not substituted.
    */
  def substitute(tpe: Type, assignments: Assignments): Type = {
    substitute(
      tpe,
      tv => assignments.get(tv) match {
        case None => tv
        case Some(t) => t
      }
    )
  }

  /**
    * Substitute occurrences of variables `tv` in the given type with a type obtained by calling `f(tv)`.
    */
  def substitute(tpe: Type, f: TypeVariable => Type): Type = {
    def rec(t: Type) = substitute(t, f)

    tpe match {
      case tv: TypeVariable => f(tv)
      case SumType(types) => SumType.construct(types.map(rec))
      case IntersectionType(types) => IntersectionType.construct(types.map(rec))
      case TupleType(elements) => TupleType(elements.map(rec))
      case FunctionType(input, output) => FunctionType(rec(input), rec(output))
      case ListType(element) => ListType(rec(element))
      case MapType(key, value) => MapType(rec(key), rec(value))
      case shapeType: ShapeType => shapeType.mapPropertyTypes(rec)
      case t => t
    }
  }

  /**
    * Returns a singleton tuple type enclosing the given type, or the type itself if it's already a tuple type.
    */
  def tupled(tpe: Type): TupleType = tpe match {
    case tpe: TupleType => tpe
    case _ => TupleType(Vector(tpe))
  }

  /**
    * Removes types from the list that are subtyped by other types in the list, essentially keeping the most specific
    * types.
    */
  def mostSpecific(types: Vector[Type]): Vector[Type] = {
    types.filterNot(t => types.exists(_ < t)).distinct
  }

  /**
    * Removes types from the list that are supertyped by other types in the list, essentially keeping the most general
    * types.
    */
  def mostGeneral(types: Vector[Type]): Vector[Type] = {
    types.filterNot(t => types.exists(t < _)).distinct
  }

  private sealed abstract class TypePrecedence(protected val value: Int) {
    def <(precedence: TypePrecedence): Boolean = this.value <= precedence.value
  }
  private object TypePrecedence {
    case object Parenthesized extends TypePrecedence(0)
    case object Sum extends TypePrecedence(1)
    case object Intersection extends TypePrecedence(2)
    case object Function extends TypePrecedence(3)
    case object Map extends TypePrecedence(4)
  }

  /**
    * Creates a pretty string representation of the type.
    */
  def toString(t: Type, verbose: Boolean = false): String = toStringWithPrecedence(t, verbose, TypePrecedence.Parenthesized)

  private def toStringWithPrecedence(t: Type, verbose: Boolean = false, parentPrecedence: TypePrecedence): String = {
    val infix = stringifyInfixOperator(parentPrecedence, toStringWithPrecedence(_, verbose, _)) _

    t match {
      case SumType(types) => infix(" | ", TypePrecedence.Sum, types.toVector)
      case IntersectionType(types) => infix(" & ", TypePrecedence.Intersection, types.toVector)
      case TupleType(elements) =>
        if (elements.isEmpty) "Unit"
        else s"(${elements.map(toString(_, verbose)).mkString(", ")})"
      case FunctionType(input, output) => infix(" => ", TypePrecedence.Function, Vector(input, output))
      case ListType(element) => s"[${toString(element, verbose)}]"
      case MapType(key, value) => infix(" -> ", TypePrecedence.Map, Vector(key, value))
      case ShapeType(properties) =>
        val propertyRepresentations = properties.values.map { property =>
          s"${property.name}: ${toString(property.tpe, verbose)}"
        }
        s"{ ${propertyRepresentations.mkString(", ")} }"
      case SymbolType(name) => s"#$name"
      case d: DeclaredType =>
        if (verbose) {
          val kind = d match {
            case _: StructType => "struct"
            case _: TraitType => "trait"
          }
          val extended = if (d.supertypes.nonEmpty) s" extends ${d.supertypes.map(toString(_, verbose)).mkString(", ")}" else ""
          s"$kind ${d.name}$extended"
        } else d.name
      case t: NamedType => t.name
      case _ => t.toString
    }
  }

  private def stringifyInfixOperator(
    parentPrecedence: TypePrecedence,
    stringify: (Type, TypePrecedence) => String,
  )(operator: String, operatorPrecedence: TypePrecedence, operands: Vector[Type]): String = {
    val repr = operands.map(stringify(_, operatorPrecedence)).mkString(operator)
    if (operatorPrecedence < parentPrecedence) s"($repr)" else repr
  }

  /**
    * Creates a unique, Javascript-friendly identifier of the given type.
    */
  def uniqueIdentifier(tpe: Type): String = uniqueIdentifier(Vector(tpe))

  /**
    * Creates a unique, Javascript-friendly identifier of the given list of types.
    *
    * The identifier is first generated as a compact binary representation of the given types individually,
    * concatenated to a single byte array, and then encoded using Base64 with '$' for the '+' character and
    * '_' for the '/' character. Padding characters are discarded as they are not needed for the identifier.
    */
  def uniqueIdentifier(types: Vector[Type]): String = {
    val stream = new ByteArrayOutputStream()
    types.foreach(t => stream.write(TypeEncoder.encode(t).toArray))
    Base64.getEncoder.encodeToString(stream.toByteArray).replace('+', '$').replace('/', '_').replace("=", "")
  }

}
