package lore.compiler.types

import com.typesafe.scalalogging.Logger
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.definitions.TypeDefinition
import lore.compiler.utils.CollectionExtensions._

/**
  * Any Lore type.
  *
  * A type is also a 0-arity (constant) [[TypeSchema]].
  *
  * Note: hashCode should be defined as a val or lazy val to avoid recomputing hashes during the runtime of the
  * compiler. At times, we will hash types heavily, and so fast hash access is important.
  */
trait Type extends TypeSchema with HasMembers {
  override def typeParameters: Vector[TypeVariable] = Vector.empty
  override def instantiate(assignments: TypeVariable.Assignments): Type = this

  def isPrimitive: Boolean = Type.isPrimitive(this)
  def isNumeric: Boolean = Type.isNumeric(this)
  def isSymbol: Boolean = Type.isSymbol(this)
  def isFunction: Boolean = Type.isFunction(this)
  def isAbstract: Boolean = Type.isAbstract(this)
  def isConcrete: Boolean = Type.isConcrete(this)
  def isPolymorphic: Boolean = Type.isPolymorphic(this)
  def isMonomorphic: Boolean = Type.isMonomorphic(this)

  def <=(rhs: Type): Boolean = Subtyping.isSubtype(this, rhs)
  def </=(rhs: Type): Boolean = !(this <= rhs)
  def <(rhs: Type): Boolean = Subtyping.isStrictSubtype(this, rhs)
  def >=(rhs: Type): Boolean = rhs <= this
  def >(rhs: Type): Boolean = rhs < this

  def fits(rhs: Type): Boolean = Fit.fits(this, rhs)
  def fitsNot(rhs: Type): Boolean = !Fit.fits(this, rhs)
}

object Type {

  val logger: Logger = Logger("lore.compiler.types")
  val loggerBlank: Logger = Logger("lore.compiler.types.blank")

  val predefinedTypes: Map[NamePath, TypeDefinition] = Vector(
    BasicType.Any,
    BasicType.Nothing,
    BasicType.Int,
    BasicType.Real,
    BasicType.Boolean,
    BasicType.String,
  ).map(t => (t.name, t)).toMap

  /**
    * Whether the given type is a primitive type, i.e. an Int, Real, Boolean, String, or symbol.
    */
  def isPrimitive(t: Type): Boolean = t match {
    case BasicType.Int | BasicType.Real | BasicType.Boolean | BasicType.String => true
    case _: SymbolType => true
    case _ => false
  }

  /**
    * Whether the given type is numeric (Int, Real).
    *
    * Note that a type `Int | Real` is <i>not</i> numeric because the sum has to be deconstructed first.
    */
  def isNumeric(t: Type): Boolean = t == BasicType.Int || t == BasicType.Real

  /**
    * Whether all values inhabiting the given type are symbols.
    *
    * TODO (syntax): This function is more or less a hack because we don't have a general symbol type that supertypes
    *                all symbols. If this was the case, we could easily check that `t` is a symbol type with the
    *                subtyping expression `t <: Symbol`.
    */
  def isSymbol(t: Type): Boolean = t match {
    case SumType(parts) => parts.forall(isSymbol)
    case _: SymbolType => true
    case _ => false
  }

  /**
    * Whether `t` is a function type.
    *
    * TODO: This doesn't take type variables or all possible intersection types into account. The better way to
    *       implement this would be checking `t <= (Nothing => Any)`, but function types expect a tuple type as their
    *       input type. Improving this situation will need larger refactoring.
    */
  def isFunction(t: Type): Boolean = t match {
    case _: FunctionType => true
    case _ => false
  }

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
      // We consider augmentations here. Consult the specification for an explanation of this approach.
      val exceptShapes = types.toVector.filterNotType[ShapeType]
      exceptShapes.nonEmpty && {
        val exceptTraits = exceptShapes.filterNotType[TraitType]
        exceptTraits.isEmpty || exceptTraits.exists(isAbstract)
      }
    case TupleType(elements) => elements.exists(isAbstract)
    case _: TraitType => true
    case struct: StructType => struct.openTypeArguments.exists(isAbstract)
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
    case t: ShapeType => t.propertyTypes.exists(isPolymorphic)
    case dt: DeclaredType => dt.typeArguments.exists(isPolymorphic)
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
    case t: ShapeType => t.propertyTypes.flatMap(variables).toSet
    case dt: DeclaredType => dt.typeArguments.flatMap(variables).toSet
    case _ => Set.empty
  }

  /**
    * Substitute occurrences of variables in the given type with values from the given assignments. Variables that
    * do not occur in the assignments are not substituted.
    */
  def substitute(tpe: Type, assignments: TypeVariable.Assignments): Type = {
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
      case FunctionType(input, output) => FunctionType(rec(input).asInstanceOf[TupleType], rec(output))
      case ListType(element) => ListType(rec(element))
      case MapType(key, value) => MapType(rec(key), rec(value))
      case shapeType: ShapeType => shapeType.mapPropertyTypes(rec)
      case dt: DeclaredType if !dt.schema.isConstantSchema => dt.schema.instantiate(dt.assignments.map { case (tv, tpe) => (tv, rec(tpe)) })
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
    * Whether `tpe` is equal to `term` or contains a subterm which is equal to `term`.
    *
    * `contains` is a faster but less feature-rich version of [[TypePath]].
    */
  def contains(tpe: Type, term: Type): Boolean = {
    if (tpe == term) {
      return true
    }

    val rec = subterm => contains(subterm, term)
    tpe match {
      case SumType(types) => types.exists(rec)
      case IntersectionType(types) => types.exists(rec)
      case TupleType(elements) => elements.exists(rec)
      case FunctionType(input, output) => rec(input) || rec(output)
      case ListType(element) => rec(element)
      case MapType(key, value) => rec(key) || rec(value)
      case tpe: ShapeType => tpe.propertyTypes.exists(rec)
      case dt: DeclaredType => dt.typeArguments.exists(rec)
      case _ => false
    }
  }

}
