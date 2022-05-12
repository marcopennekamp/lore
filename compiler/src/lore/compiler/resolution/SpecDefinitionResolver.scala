package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.syntax.DeclNode

object SpecDefinitionResolver {

  def resolve(node: DeclNode.SpecNode)(implicit types: Registry.Types, bindings: Registry.Bindings, reporter: Reporter): SpecDefinition = {
    new SpecDefinition(node.fullName, node.isTest, node.isBenchmark, node.body, node.localModule, node.position)
  }

}
