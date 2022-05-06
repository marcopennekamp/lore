package lore.compiler.poem

import lore.compiler.types.{BasicType, DeclaredSchema, Kind}

sealed trait PoemType

case class PoemTypeVariable(index: Int) extends PoemType
case class PoemBasicType(underlying: BasicType) extends PoemType
case class PoemSymbolType(name: String) extends PoemType
case class PoemXaryType(kind: Kind, types: Vector[PoemType]) extends PoemType
case class PoemShapeType(properties: Map[String, PoemType]) extends PoemType with Poem.SortedProperties[PoemType]
case class PoemNamedType(schema: DeclaredSchema, typeArguments: Vector[PoemType]) extends PoemType
