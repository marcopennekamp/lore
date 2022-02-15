package lore.compiler.resolution

import lore.compiler.core.UniqueKey
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.functions.ParameterDefinition
import lore.compiler.semantics.scopes.{BindingScope, TypeScope}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.BasicType

object ParameterDefinitionResolver {

  def resolve(node: DeclNode.ParameterNode)(implicit typeScope: TypeScope, bindingScope: BindingScope, reporter: Reporter): ParameterDefinition = {
    val tpe = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Any)
    ParameterDefinition(UniqueKey.fresh(), node.name, tpe, node.nameNode.map(_.position).getOrElse(node.position))
  }

}
