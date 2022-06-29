package lore.compiler.resolution

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.TraitDefinition
import lore.compiler.syntax.DeclNode
import lore.compiler.types.TraitSchema

object TraitSchemaResolver {

  // TODO (multi-import): Move error to central location.
  case class TraitIllegalExtends(node: DeclNode.TraitNode) extends Feedback.Error(node) {
    override def message = s"The trait ${node.fullName} does not extend a trait or shape but some other type."
  }

  def resolve(node: DeclNode.TraitNode)(implicit registry: Registry, reporter: Reporter): TraitSchema = {
    Resolver.withTypeParameters(node.localModule, node.typeVariables) {
      implicit typeScope => implicit termScope => typeParameters =>
        val supertypes = InheritanceResolver.resolveInheritedTypes(node.extended, TraitIllegalExtends(node))
        val schema = new TraitSchema(node.fullName, typeParameters, supertypes)
        val definition = new TraitDefinition(node.fullName, schema, node.localModule, node.nameNode.position)
        schema.initialize(definition)
        schema

    }
  }

}
