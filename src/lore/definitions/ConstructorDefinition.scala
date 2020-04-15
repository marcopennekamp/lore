package lore.definitions

import lore.ast.ExprNode

case class ConstructorDefinition(name: String, parameters: List[ParameterDefinition], body: ExprNode.BlockNode)
