package lore.definitions

import lore.ast.ExprNode
import lore.compiler.feedback.Position
import lore.types.ProductType

case class ConstructorDefinition(
  name: String, parameters: List[ParameterDefinition], bodyBlock: ExprNode.BlockNode,
  override val position: Position
) extends CallTarget {
  override val body: Option[ExprNode.BlockNode] = Some(bodyBlock)
  override lazy val signature: FunctionSignature = FunctionSignature(name, parameters, ProductType.UnitType)
}
