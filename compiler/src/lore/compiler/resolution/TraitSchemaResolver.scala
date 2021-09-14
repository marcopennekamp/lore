package lore.compiler.resolution

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.TraitSchema

object TraitSchemaResolver {

  case class TraitIllegalExtends(node: DeclNode.TraitNode) extends Feedback.Error(node) {
    override def message = s"The trait ${node.name} does not extend a trait or shape but some other type."
  }

  def resolve(node: DeclNode.TraitNode, parentScope: TypeScope)(implicit reporter: Reporter): TraitSchema = {
    val typeParameters = TypeVariableResolver.resolve(node.typeVariables, parentScope)
    implicit val typeScope: TypeScope = ImmutableTypeScope.from(typeParameters, parentScope)
    val supertypes = InheritanceResolver.resolveInheritedTypes(node.extended, TraitIllegalExtends(node))
    new TraitSchema(node.name, typeParameters, supertypes)
  }

}
