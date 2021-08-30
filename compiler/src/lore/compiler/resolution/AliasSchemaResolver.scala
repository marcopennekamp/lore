package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types._

object AliasSchemaResolver {

  def resolve(node: TypeDeclNode.AliasNode, parentTypeScope: TypeScope)(implicit reporter: Reporter): AliasSchema = {
    val typeParameters = TypeVariableResolver.resolve(node.typeVariables, parentTypeScope)
    implicit val typeScope: TypeScope = ImmutableTypeScope.from(typeParameters, parentTypeScope)
    val originalType = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Any)
    new AliasSchema(node.name, typeParameters, originalType)
  }

}
