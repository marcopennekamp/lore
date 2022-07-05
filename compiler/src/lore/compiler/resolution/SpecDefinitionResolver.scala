package lore.compiler.resolution

import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.syntax.DeclNode

object SpecDefinitionResolver {

  def create(node: DeclNode.SpecNode): SpecDefinition = {
    new SpecDefinition(node.description, node.isTest, node.isBenchmark, node)
  }

}
