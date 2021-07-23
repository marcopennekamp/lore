package lore.compiler.types

import lore.compiler.semantics.members.Member
import lore.compiler.semantics.structures.StructPropertyDefinition

import scala.util.hashing.MurmurHash3

case class ShapeType(properties: Map[String, ShapeType.Property]) extends Type {

  /**
    * Maps the given function to each property type, constructing a new shape type in the process.
    */
  def mapPropertyTypes(f: Type => Type): ShapeType = ShapeType(properties.values.map(_.mapType(f)))

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

  override val hashCode: Int = MurmurHash3.unorderedHash(properties.values, 0xf38da2c4)

}

object ShapeType {

  val empty: ShapeType = ShapeType(Map.empty[String, Property])

  /**
    * Creates a new shape type from the given list of properties. If the shape type would be empty, the function instead
    * returns the common [[ShapeType.empty]] instance.
    */
  def apply(properties: Iterable[Property]): ShapeType = {
    if (properties.nonEmpty) {
      ShapeType(properties.map(property => (property.name, property)).toMap)
    } else empty
  }

  def apply(properties: (String, Type)*): ShapeType = apply(properties.map { case (name, tpe) => ShapeType.Property(name, tpe) })

  case class Property(name: String, tpe: Type) {
    def mapType(f: Type => Type): ShapeType.Property = this.copy(tpe = f(tpe))

    def asMember: Member = Member(name, tpe)
  }

  object Property {
    def apply(property: StructPropertyDefinition.Instance): Property = Property(property.definition.name, property.tpe)
  }

  /**
    * Combines the given list of shape types into a single shape type. Property types are combined with an
    * intersection. Consider the following example:
    *
    *     { x: A } & { x: B } = { x: A & B }
    *
    * This treatment of property types leads to the correct subtyping behavior:
    *
    *     { x: A & B } <: { x: A }
    *     { x: A & B } <: { x: B }
    *
    * Which needs to hold, because looking at it without the shape type we get:
    *
    *     X & Y = Z
    *     Z <: X
    *     Z <: Y
    *
    * Which are the standard intersection type subtyping rules.
    */
  def combine(shapes: Vector[ShapeType]): ShapeType = {
    val combinedProperties = shapes.flatMap(_.properties.values).groupBy(_.name).map {
      case (name, properties) => Property(name, IntersectionType.construct(properties.map(_.tpe)))
    }
    ShapeType(combinedProperties)
  }

  /**
    * Lists all properties that this shape type and another have in common.
    */
  def common(s1: ShapeType, s2: ShapeType): Vector[(ShapeType.Property, ShapeType.Property)] = {
    s1.correlate(s2).flatMap { case (p1, maybeP2) => maybeP2.map(p2 => (p1, p2)) }
  }

  /**
    * Correlates each property from s1 with a property from s2 and vice versa.
    */
  def bicorrelate(s1: ShapeType, s2: ShapeType): Vector[(Option[ShapeType.Property], Option[ShapeType.Property])] = {
    val left = s1.correlate(s2).map {
      case (p1, maybeP2) => (Some(p1), maybeP2)
    }

    // We only have to take properties that aren't already in s1, because the other results will already be included in
    // `left`. It suffices to take all tuples where the correlated property is None.
    val right = s2.correlate(s1).filter(_._2.isEmpty).map {
      case (p2, None) => (None, Some(p2))
    }

    left ++ right
  }

}
