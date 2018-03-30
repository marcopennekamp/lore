package lore.ast

case class FunctionDeclaration(name: String, parameters: Seq[ParameterDeclaration], isAbstract: Boolean) extends TopLevelElement
