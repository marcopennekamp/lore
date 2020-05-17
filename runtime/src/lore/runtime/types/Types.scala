package lore.runtime.types

import lore.types.{AnyType, BasicType, IntersectionType, ListType, MapType, NothingType, ProductType, SumType, Type}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
  * Exposes type construction, verification, and decision functions to the generated JS code.
  */
@JSExportTopLevel("Types")
@JSExportAll
object Types {
  def typeof(value: Any): Type = {
    js.typeOf(value) match {
      case _ => println(value); any
    }
  }

  // TODO: We definitely need a simple type registry with (name -> declared type).
  def registerClassType(
    name: String, supertype: Option[ClassType], ownedBy: Option[Type],
    isAbstract: Boolean, isEntity: Boolean, componentTypes: List[ComponentType],
  ): Unit = {
    ClassType(name, supertype, ownedBy, isAbstract, isEntity, componentTypes)
  }

  def registerLabelType(name: String, supertype: Option[LabelType]): Unit = {
    LabelType(name, supertype)
  }

  // Leaf types.
  def any: Type = AnyType
  def nothing: Type = NothingType
  def real: BasicType = BasicType.Real
  def int: BasicType = BasicType.Int
  def boolean: BasicType = BasicType.Boolean
  def string: BasicType = BasicType.String
  def declaredType(name: String): DeclaredType = ??? // TODO: Fetch the registered declared type.

  // Type constructors.
  def intersection(types: List[Type]): Type = IntersectionType.construct(types)
  def sum(types: List[Type]): Type = SumType.construct(types)
  def product(types: List[Type]): ProductType = ProductType(types)
  def component(underlying: ClassType): ComponentType = ComponentType(underlying)
  def list(element: Type): ListType = ListType(element)
  def map(key: Type, value: Type): MapType = MapType(key, value)
}
