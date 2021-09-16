package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.AliasDefinition
import lore.compiler.syntax.DeclNode
import lore.compiler.types.AliasSchema
import lore.compiler.utils.CollectionExtensions.OptionExtension

object AliasDefinitionResolver {

  def resolve(node: DeclNode.AliasNode)(implicit types: Registry.Types, reporter: Reporter): AliasDefinition = {
    val schema = types.schemas.get(node.fullName).filterType[AliasSchema].getOrElse(
      throw CompilationException(s"The type schema for alias ${node.fullName} should be registered by now.")
    )
    new AliasDefinition(node.fullName, schema, node.localModule, node.nameNode.position)
  }

}
