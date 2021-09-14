package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.semantics.structures.AliasDefinition
import lore.compiler.syntax.DeclNode

object AliasDefinitionResolver {

  def resolve(node: DeclNode.AliasNode, parentScope: TypeScope)(implicit reporter: Reporter): AliasDefinition = {
    val schema = parentScope.getAliasSchema(node.name).getOrElse(
      throw CompilationException(s"The type schema for alias ${node.name} should be registered by now.")
    )
    new AliasDefinition(node.name, schema, node.nameNode.position)
  }

}
