package lore.ast

case class FunctionDeclaration(name: String, parameters: List[ParameterDeclaration], isAbstract: Boolean) extends TopLevelElement
