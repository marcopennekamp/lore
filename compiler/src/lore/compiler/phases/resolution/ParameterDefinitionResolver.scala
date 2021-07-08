package lore.compiler.phases.resolution

import lore.compiler.core.Compilation
import lore.compiler.semantics.functions.ParameterDefinition
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.DeclNode

object ParameterDefinitionResolver {

  /**
    * Resolves a parameter definition from the given parameter node.
    */
  def resolveParameterNode(node: DeclNode.ParameterNode)(implicit typeScope: TypeScope): Compilation.Result[ParameterDefinition] = {
    TypeExpressionEvaluator.evaluate(node.tpe).map(t => ParameterDefinition(node.name, t, node.position))
  }

}
