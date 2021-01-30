package lore.compiler.phases.resolution

import lore.compiler.core.Compilation
import lore.compiler.semantics.functions.ParameterDefinition
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.DeclNode

object ParameterDefinitionResolver {
  def resolveParameterNode(node: DeclNode.ParameterNode)(implicit typeScope: TypeScope): Compilation[ParameterDefinition] = {
    TypeExpressionEvaluator.evaluate(node.tpe).map(t => ParameterDefinition(node.name, t, node.position))
  }
}
