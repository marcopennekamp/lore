package lore.compiler.functions

import lore.compiler.ast.ExprNode
import lore.compiler.core.TypeScope
import lore.compiler.feedback.Position
import lore.compiler.functions
import lore.compiler.structures._

case class ConstructorDefinition(
  override val name: String, typeScope: TypeScope, parameters: List[ParameterDefinition],
  body: ExprNode.BlockNode, override val position: Position
) extends InternalCallTarget {
  private var classDefinition: ClassDefinition = _
  def associateWith(classDefinition: ClassDefinition): Unit = {
    this.classDefinition = classDefinition
  }
  override lazy val signature: FunctionSignature = functions.FunctionSignature(name, parameters, classDefinition.tpe, position)
}
