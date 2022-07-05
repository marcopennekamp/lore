package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.syntax.DeclNode.TraitNode
import lore.compiler.types.TraitSchema

object TraitSchemaResolver {

  /**
    * Creates an uninitialized [[TraitSchema]] for `node`. Local modules of nodes are not yet resolved at this point.
    */
  def create(
    node: TraitNode,
    globalModule: GlobalModule,
  )(implicit registry: Registry, reporter: Reporter): TraitSchema = {
    new TraitSchema(globalModule.name + node.simpleName, node)
  }

  /**
    * Initializes `schema`. (See the guidelines in [[lore.compiler.semantics.definitions.BindingDefinition]].)
    */
  def initialize(schema: TraitSchema)(implicit registry: Registry, reporter: Reporter): Unit = {
    DeclaredSchemaResolver.initialize(schema, schema.node)
  }

}
