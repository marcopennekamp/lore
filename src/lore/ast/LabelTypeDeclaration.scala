package lore.ast

case class LabelTypeDeclaration(name: String, supertypeName: Option[String]) extends TopLevelElement
