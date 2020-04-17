package lore.definitions

import lore.ast.ExprNode
import lore.compiler.Position

case class ConstructorDefinition(
  name: String, parameters: List[ParameterDefinition], body: ExprNode.BlockNode,
  override val position: Position
) extends PositionedDefinition
