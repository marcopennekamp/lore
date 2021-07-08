package lore.compiler.phases.resolution

import lore.compiler.core.Compilation
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types._

object AliasTypeResolver {

  def resolve(node: TypeDeclNode.AliasNode)(implicit typeScope: TypeScope): Compilation.Result[Type] = {
    TypeExpressionEvaluator.evaluate(node.tpe)
  }

}
