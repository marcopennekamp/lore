package lore.compiler.resolution

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.syntax.DeclNode
import lore.compiler.types.StructSchema

object StructSchemaResolver {

  case class StructIllegalExtends(node: DeclNode.StructNode) extends Feedback.Error(node) {
    override def message = s"The struct ${node.fullName} does not implement a trait or shape but some other type."
  }

  def resolve(node: DeclNode.StructNode)(implicit types: Registry.Types, bindings: Registry.Bindings, reporter: Reporter): StructSchema = {
    Resolver.withTypeParameters(node.localModule, node.typeVariables) {
      implicit typeScope => implicit bindingScope => typeParameters =>
        val supertypes = InheritanceResolver.resolveInheritedTypes(node.extended, StructIllegalExtends(node))
        new StructSchema(node.fullName, typeParameters, supertypes)
    }
  }

}
