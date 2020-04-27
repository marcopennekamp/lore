package lore.definitions

import lore.ast.ExprNode
import lore.compiler.feedback.Position
import lore.types.ProductType

case class ConstructorDefinition(
  name: String, parameters: List[ParameterDefinition], body: ExprNode.BlockNode,
  override val position: Position
) extends PositionedDefinition {
  lazy val signature: FunctionSignature = FunctionSignature(name, parameters, ProductType.UnitType)
}
