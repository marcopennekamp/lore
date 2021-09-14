package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.semantics.structures.TraitDefinition
import lore.compiler.syntax.DeclNode

object TraitDefinitionResolver {

  def resolve(node: DeclNode.TraitNode, parentScope: TypeScope): TraitDefinition = {
    val schema = parentScope.getTraitSchema(node.name).getOrElse(
      throw CompilationException(s"The type schema for trait ${node.name} should be registered by now.")
    )

    val definition = new TraitDefinition(node.name, schema, node.nameNode.position)
    schema.initialize(definition)
    definition
  }

}
