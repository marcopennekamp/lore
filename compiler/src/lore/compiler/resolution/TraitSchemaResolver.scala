package lore.compiler.resolution

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.syntax.DeclNode
import lore.compiler.types.TraitSchema

object TraitSchemaResolver {

  case class TraitIllegalExtends(node: DeclNode.TraitNode) extends Feedback.Error(node) {
    override def message = s"The trait ${node.fullName} does not extend a trait or shape but some other type."
  }

  def resolve(node: DeclNode.TraitNode)(implicit types: Registry.Types, bindings: Registry.Bindings, reporter: Reporter): TraitSchema = {
    Resolver.withTypeParameters(node.localModule, node.typeVariables) {
      implicit typeScope => implicit bindingScope => typeParameters =>
        val supertypes = InheritanceResolver.resolveInheritedTypes(node.extended, TraitIllegalExtends(node))
        new TraitSchema(node.fullName, typeParameters, supertypes)
    }
  }

}
