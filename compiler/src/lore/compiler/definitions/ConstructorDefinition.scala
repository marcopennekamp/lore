package lore.compiler.definitions

import lore.compiler.ast.ExprNode
import lore.compiler.feedback.Position

case class ConstructorDefinition(
  name: String, parameters: List[ParameterDefinition], bodyBlock: ExprNode.BlockNode,
  override val position: Position
) extends CallTarget {
  private var classDefinition: ClassDefinition = _
  def associateWith(classDefinition: ClassDefinition): Unit = {
    this.classDefinition = classDefinition
  }
  override val body: Option[ExprNode.BlockNode] = Some(bodyBlock)
  override lazy val signature: FunctionSignature = FunctionSignature(name, parameters, classDefinition.tpe)
}
