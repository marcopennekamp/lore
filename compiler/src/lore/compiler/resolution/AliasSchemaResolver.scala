package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.AliasDefinition
import lore.compiler.syntax.DeclNode.AliasNode
import lore.compiler.types._

object AliasSchemaResolver {

  def resolve(node: AliasNode)(implicit registry: Registry, reporter: Reporter): AliasSchema = {
    Resolver.withTypeParameters(node.localModule, node.typeVariables) {
      implicit typeScope => implicit termScope => typeParameters =>
        val originalType = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Any)
        val schema = new AliasSchema(node.fullName, typeParameters, originalType)
        val definition = new AliasDefinition(
          node.fullName,
          schema,
          node.isStructAlias,
          node.localModule,
          node.nameNode.position,
        )
        schema.initialize(definition)
        schema
    }
  }

}
