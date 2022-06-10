package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.syntax.DeclNode
import lore.compiler.types._

object AliasSchemaResolver {

  def resolve(node: DeclNode.AliasNode)(implicit types: Registry.Types, terms: Registry.Terms, reporter: Reporter): AliasSchema = {
    Resolver.withTypeParameters(node.localModule, node.typeVariables) {
      implicit typeScope => implicit termScope => typeParameters =>
        val originalType = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Any)
        new AliasSchema(node.fullName, typeParameters, originalType)
    }
  }

}
