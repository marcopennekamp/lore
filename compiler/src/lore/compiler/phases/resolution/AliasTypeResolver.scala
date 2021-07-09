package lore.compiler.phases.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types._

object AliasTypeResolver {

  def resolve(node: TypeDeclNode.AliasNode)(implicit typeScope: TypeScope, reporter: Reporter): Type = {
    TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Any)
  }

}
