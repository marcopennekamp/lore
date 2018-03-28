package lore.ast

case class FunctionDeclaration(name: String, parameters: Seq[ParameterDeclaration]) extends TopLevelElement
