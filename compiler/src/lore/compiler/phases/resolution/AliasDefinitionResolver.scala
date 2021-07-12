package lore.compiler.phases.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.semantics.structures.AliasDefinition
import lore.compiler.syntax.TypeDeclNode

object AliasDefinitionResolver {

  def resolve(node: TypeDeclNode.AliasNode)(implicit typeScope: TypeScope, reporter: Reporter): AliasDefinition = {
    val tpe = typeScope.get(node.name).getOrElse(
      throw CompilationException(s"The type for alias ${node.name} should be registered by now.")
    )
    new AliasDefinition(node.name, tpe, node.position)
  }

}
