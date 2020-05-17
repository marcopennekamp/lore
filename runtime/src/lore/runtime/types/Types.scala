package lore.runtime.types

import lore.types.{AnyType, BasicType, IntersectionType, ListType, MapType, NothingType, ProductType, Subtyping, SumType, Type}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
  * Exposes type construction, verification, and decision functions to the generated JS code.
  */
@JSExportTopLevel("Types")
@JSExportAll
object Types {
  /**
    * Calculates the Lore type of a Javascript value.
    */
  def typeof(value: Any): Type = {
    // TODO: In the case of a Javascript object being given that does not have a type field, we should return some
    //       kind of "dynamic" type. Of course, this first requires us to define a similar notion within Lore
    //       itself.
    js.typeOf(value) match {
      case "number" => value match {
        case _: Int => BasicType.Int
        case _: Double => BasicType.Real
      }
      case "boolean" => BasicType.Boolean
      case "string" => BasicType.String
      case "object" =>
        // TODO: Get the type from the object, for example in an value.$type property.
        //       This is ALSO the case for arrays! We can't deduce an array's type if it has no elements,
        //       and so we have to wrap it in an object that also contains type information.
        any
      case _ => println(value); any // TODO: Throw a "corresponding Lore type not found" error.
    }
  }

  def isSubtype(left: Type, right: Type): Boolean = left <= right

  // TODO: We definitely need a simple type registry with (name -> declared type).
  def registerClass(
    name: String, supertype: Option[ClassType], ownedBy: Option[Type],
    isAbstract: Boolean, isEntity: Boolean, componentTypes: List[ComponentType],
  ): Unit = {
    ClassType(name, supertype, ownedBy, isAbstract, isEntity, componentTypes)
  }

  def registerLabel(name: String, supertype: Option[LabelType]): Unit = {
    LabelType(name, supertype)
  }

  // Leaf types.
  def any: Type = AnyType
  def nothing: Type = NothingType
  def real: BasicType = BasicType.Real
  def int: BasicType = BasicType.Int
  def boolean: BasicType = BasicType.Boolean
  def string: BasicType = BasicType.String
  def declared(name: String): DeclaredType = ??? // TODO: Fetch the registered declared type.

  // Type constructors.
  def intersection(types: js.Array[Type]): Type = IntersectionType.construct(types.toList)
  def sum(types: js.Array[Type]): Type = SumType.construct(types.toList)
  def product(types: js.Array[Type]): ProductType = ProductType(types.toList)
  def component(underlying: ClassType): ComponentType = ComponentType(underlying)
  def list(element: Type): ListType = ListType(element)
  def map(key: Type, value: Type): MapType = MapType(key, value)
}
