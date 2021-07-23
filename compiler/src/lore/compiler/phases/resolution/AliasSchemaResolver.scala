package lore.compiler.phases.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.scopes.{LocalTypeScope, TypeScope}
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types._

object AliasSchemaResolver {

  def resolve(node: TypeDeclNode.AliasNode, parentTypeScope: TypeScope)(implicit reporter: Reporter): AliasSchema = {
    implicit val typeScope: LocalTypeScope = TypeVariableDeclarationResolver.resolve(node.typeVariables, parentTypeScope)
    val originalType = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Any)
    new AliasSchema(node.name, typeScope, originalType)
  }

}
