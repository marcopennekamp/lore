package lore.compiler.resolution

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.StructDefinition
import lore.compiler.syntax.DeclNode
import lore.compiler.types.StructSchema

object StructSchemaResolver {

  // TODO (multi-import): Move error to central location.
  case class StructIllegalExtends(node: DeclNode.StructNode) extends Feedback.Error(node) {
    override def message = s"The struct ${node.fullName} does not implement a trait or shape but some other type."
  }

  def resolve(node: DeclNode.StructNode)(implicit registry: Registry, reporter: Reporter): StructSchema = {
    Resolver.withTypeParameters(node.localModule, node.typeVariables) {
      implicit typeScope => implicit termScope => typeParameters =>
        val supertypes = InheritanceResolver.resolveInheritedTypes(node.extended, StructIllegalExtends(node))
        val schema = new StructSchema(node.fullName, typeParameters, supertypes)
        val companionModule = registry.getModule(node.fullName)
        val definition = new StructDefinition(
          node.fullName,
          schema,
          node.isObject,
          companionModule,
          node.localModule,
          node.nameNode.position,
        )
        schema.initialize(definition)
        schema
    }
  }

}
