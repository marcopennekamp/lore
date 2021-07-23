package lore.compiler.types

import scala.util.hashing.MurmurHash3

trait DeclaredType extends NamedType {

  /**
    * The schema this declared type was instantiated from.
    */
  def schema: DeclaredSchema

  override def name: String = schema.name

  /**
    * The type arguments this declared type has been instantiated with.
    */
  def typeArguments: TypeVariable.Assignments

  /**
    * The inherited shape type of the declared type is derived from the schema's inherited shape type, but with all
    * type parameters instantiated given the current type arguments.
    */
  lazy val inheritedShapeType: ShapeType = Type.substitute(schema.inheritedShapeType, typeArguments).asInstanceOf[ShapeType]

  /**
    * The declared type viewed as a compile-time shape type. By default, this is equal to the [[inheritedShapeType]].
    * Structs, however, implement `asShapeType` based on their properties.
    */
  def asShapeType: ShapeType = inheritedShapeType

}
