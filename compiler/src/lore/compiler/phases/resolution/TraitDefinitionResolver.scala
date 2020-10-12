package lore.compiler.phases.resolution

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.TraitDefinition
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types.BasicType

object TraitDefinitionResolver {
  def resolve(node: TypeDeclNode.TraitNode)(implicit registry: Registry): Compilation[TraitDefinition] = {
    val traitType = registry.getTraitType(node.name).getOrElse(
      throw CompilationException(s"The trait type for trait ${node.name} should be registered by now.")
    )

    node.ownedBy.map(TypeExpressionEvaluator.evaluate(_)(registry.typeScope)).toCompiledOption.map { ownedBy =>
      val definition = new TraitDefinition(node.name, traitType, ownedBy.getOrElse(BasicType.Any), node.position)
      traitType.initialize(definition)
      definition
    }
  }
}
