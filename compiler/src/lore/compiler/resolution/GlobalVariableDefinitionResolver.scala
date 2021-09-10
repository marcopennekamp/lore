package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.syntax.DeclNode
import lore.compiler.types.BasicType

object GlobalVariableDefinitionResolver {

  def resolve(node: DeclNode.GlobalVariableNode)(implicit typeScope: TypeScope, reporter: Reporter): GlobalVariableDefinition = {
    val tpe = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Nothing)
    new GlobalVariableDefinition(node.name, tpe, node.value, node.position)
  }

}
