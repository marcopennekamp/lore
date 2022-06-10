package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.TraitDefinition
import lore.compiler.syntax.DeclNode
import lore.compiler.types.TraitSchema
import lore.compiler.utils.CollectionExtensions.OptionExtension

object TraitDefinitionResolver {

  def resolve(node: DeclNode.TraitNode)(implicit types: Registry.Types, terms: Registry.Terms): TraitDefinition = {
    Resolver.withRegistryScopes(node.localModule) {
      implicit typeScope => implicit termScope =>
        val schema = types.schemas.get(node.fullName).filterType[TraitSchema].getOrElse(
          throw CompilationException(s"The type schema for trait ${node.fullName} should be registered by now.")
        )
        val definition = new TraitDefinition(node.fullName, schema, node.localModule, node.nameNode.position)
        schema.initialize(definition)
        definition
    }
  }

}
