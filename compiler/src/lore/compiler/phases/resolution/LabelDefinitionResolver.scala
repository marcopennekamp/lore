package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.{C, ToCompilationExtension}
import lore.compiler.core.CompilationException
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.LabelDefinition
import lore.compiler.syntax.TypeDeclNode

object LabelDefinitionResolver {
  def resolve(node: TypeDeclNode.LabelNode)(implicit registry: Registry): C[LabelDefinition] = {
    val labelType = registry.getLabelType(node.name).getOrElse(
      throw CompilationException(s"The label type for label ${node.name} should be registered by now!")
    )
    val definition = new LabelDefinition(node.name, labelType, node.position)
    labelType.initialize(definition)
    definition.compiled
  }
}
