package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.syntax.DeclNode
import lore.compiler.types.BasicType

object GlobalVariableDefinitionResolver {

  def resolve(node: DeclNode.GlobalVariableNode)(implicit types: Registry.Types, bindings: Registry.Bindings, reporter: Reporter): GlobalVariableDefinition = {
    Resolver.withRegistryScopes(node.localModule) {
      implicit typeScope => implicit bindingScope =>
        val tpe = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Nothing)
        new GlobalVariableDefinition(node.fullName, tpe, node.value, node.position)
    }
  }

}
