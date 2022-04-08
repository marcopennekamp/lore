package lore.compiler.poem

import lore.compiler.semantics.NamePath
import lore.compiler.types.Kind

sealed trait PoemSchema {
  def kind: Kind
  def name: NamePath
  def typeParameters: Vector[PoemTypeParameter]
  def supertraits: Vector[PoemNamedType]
}

case class PoemTraitSchema(
  kind: Kind,
  name: NamePath,
  typeParameters: Vector[PoemTypeParameter],
  supertraits: Vector[PoemNamedType],
  inheritedShapeType: PoemShapeType,
) extends PoemSchema

/**
  * @param properties The struct's properties must be ordered lexicographically by their name.
  */
case class PoemStructSchema(
  kind: Kind,
  name: NamePath,
  typeParameters: Vector[PoemTypeParameter],
  supertraits: Vector[PoemNamedType],
  properties: Vector[PoemStructProperty],
) extends PoemSchema

case class PoemStructProperty(name: String, tpe: PoemType, isOpen: Boolean, declarationIndex: Int)
