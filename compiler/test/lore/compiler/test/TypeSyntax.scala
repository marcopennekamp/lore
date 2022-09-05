package lore.compiler.test

import lore.compiler.core.UniqueKey
import lore.compiler.semantics.Registry
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.types._

/**
  * This type syntax can be used to specify types with minimal syntax noise. Only intended for test and benchmark
  * applications.
  */
trait TypeSyntax {
  val any: BasicType = BasicType.Any
  val nothing: BasicType = BasicType.Nothing
  val int: BasicType = BasicType.Int
  val real: BasicType = BasicType.Real
  val boolean: BasicType = BasicType.Boolean
  val string: BasicType = BasicType.String

  /**
    * Constructs a new type variable. Specifying the `index` is mandatory because [[Fit]] requires it to bring type
    * variables into the correct dependency order. For example, if we have type variables `A` and `B >: A`, `A` must be
    * ordered before `B`, so `A` must have index 0 and `B` must have index 1.
    */
  def typeVariable(
    name: String,
    index: Int,
    lowerBound: Type = BasicType.Nothing,
    upperBound: Type = BasicType.Any,
  ): TypeVariable = {
    new TypeVariable(UniqueKey.fresh(), name, lowerBound, upperBound, Variance.Invariant, false, index)
  }

  implicit def toType(name: String)(implicit registry: Registry): Type = registry.rootModule.types.get(name).get.constantType

  def tuple(elements: Type*): TupleType = TupleType(elements.toVector)

  implicit def toTupleTypeSS(tuple: (String, String))(implicit registry: Registry): TupleType = toTupleTypeTT((toType(tuple._1), toType(tuple._2)))
  implicit def toTupleTypeTS(tuple: (Type, String))(implicit registry: Registry): TupleType = toTupleTypeTT((tuple._1, toType(tuple._2)))
  implicit def toTupleTypeST(tuple: (String, Type))(implicit registry: Registry): TupleType = toTupleTypeTT((toType(tuple._1), tuple._2))
  implicit def toTupleTypeTT(tuple: (Type, Type)): TupleType = TupleType(Vector(tuple._1, tuple._2))
  implicit def toTupleTypeTTT(tuple: (Type, Type, Type)): TupleType = TupleType(Vector(tuple._1, tuple._2, tuple._3))

  def list(element: Type): ListType = ListType(element)

  def shape(properties: (String, Type)*): ShapeType = ShapeType(properties: _*)

  implicit class TypeOperators(t1: Type) {
    def &(t2: Type): Type = IntersectionType.construct(t1, t2)
    def |(t2: Type): Type = SumType.construct(t1, t2)
    def #>(t2: Type): Type = MapType(t1, t2)
  }

  implicit class StringTypeOperators(s1: String) {
    def &(t2: Type)(implicit registry: Registry): Type = toType(s1) & t2
    def |(t2: Type)(implicit registry: Registry): Type = toType(s1) | t2
  }

  implicit class DeclaredSchemaExtension(schema: DeclaredSchema) {
    implicit def apply(arguments: Type*): DeclaredType = schema.instantiate(arguments.toVector).get.asInstanceOf[DeclaredType]
  }
}
