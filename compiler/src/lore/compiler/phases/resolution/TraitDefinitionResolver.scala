package lore.compiler.phases.resolution

import lore.compiler.core.CompilationException
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.semantics.structures.TraitDefinition
import lore.compiler.syntax.TypeDeclNode

object TraitDefinitionResolver {

  def resolve(node: TypeDeclNode.TraitNode)(implicit typeScope: TypeScope): TraitDefinition = {
    val traitType = typeScope.getTraitSchema(node.name).getOrElse(
      throw CompilationException(s"The trait type for trait ${node.name} should be registered by now.")
    )

    val definition = new TraitDefinition(node.name, traitType, node.nameNode.position)
    traitType.initialize(definition)
    definition
  }

}
