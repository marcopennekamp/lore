package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.syntax.DeclNode.GlobalVariableNode
import lore.compiler.types.BasicType

object GlobalVariableDefinitionResolver {

  /**
    * Creates an uninitialized [[GlobalVariableNode]] for `node`. Local modules of nodes are not yet resolved at this
    * point.
    */
  def create(
    node: GlobalVariableNode,
    globalModule: GlobalModule,
  )(implicit registry: Registry, reporter: Reporter): GlobalVariableDefinition = {
    new GlobalVariableDefinition(globalModule.name + node.simpleName, node)
  }

  /**
    * Initializes `variable`. (See the guidelines in [[lore.compiler.semantics.definitions.BindingDefinition]].)
    */
  def initialize(variable: GlobalVariableDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    Resolver.withRegistryScopes(variable.localModule) {
      implicit typeScope => implicit termScope =>
        val tpe = TypeExpressionEvaluator.evaluate(variable.node.tpe).getOrElse(BasicType.Nothing)
        variable.initialize(tpe)
    }
  }

}
