package lore.compiler.phases.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.functions.ParameterDefinition
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.DeclNode
import lore.compiler.types.BasicType

object ParameterDefinitionResolver {

  def resolve(node: DeclNode.ParameterNode)(implicit typeScope: TypeScope, reporter: Reporter): ParameterDefinition = {
    val tpe = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Any)
    ParameterDefinition(node.name, tpe, node.position)
  }

}
