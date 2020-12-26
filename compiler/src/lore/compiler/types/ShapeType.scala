package lore.compiler.types

import lore.compiler.semantics.structures.StructPropertyDefinition

case class ShapeType(properties: Map[String, ShapeType.Property]) extends Type {

  /**
    * Correlates each property from this shape type with a property from the other shape type.
    */
  def correlate(other: ShapeType): Vector[(ShapeType.Property, Option[ShapeType.Property])] = {
    properties.values.map { p1 =>
      other.properties.get(p1.name) match {
        case Some(p2) => (p1, Some(p2))
        case None => (p1, None)
      }
    }.toVector
  }

  /**
    * Lists all properties that this shape type and another have in common.
    */
  def common(other: ShapeType): Vector[(ShapeType.Property, ShapeType.Property)] = {
    correlate(other).flatMap { case (p1, maybeP2) => maybeP2.map(p2 => (p1, p2)) }
  }

}

object ShapeType {

  def apply(properties: Iterable[Property]): ShapeType = ShapeType(properties.map(property => (property.name, property)).toMap)

  case class Property(name: String, tpe: Type) {
    def mapType(f: Type => Type): ShapeType.Property = this.copy(tpe = f(tpe))
  }

  object Property {
    def apply(definition: StructPropertyDefinition): Property = Property(definition.name, definition.tpe)
  }

}
