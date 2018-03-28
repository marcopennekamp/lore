package lore.ast

case class TypeDeclaration(name: String, supertypeName: Option[String]) extends TopLevelElement
