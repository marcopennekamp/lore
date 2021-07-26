package lore.compiler.phases.resolution

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types.TraitSchema

object TraitSchemaResolver {

  case class TraitIllegalExtends(node: TypeDeclNode.TraitNode) extends Feedback.Error(node) {
    override def message = s"The trait ${node.name} does not extend a trait or shape but some other type."
  }

  def resolve(node: TypeDeclNode.TraitNode, parentScope: TypeScope)(implicit reporter: Reporter): TraitSchema = {
    val typeParameters = TypeVariableDeclarationResolver.resolve(node.typeVariables, parentScope)
    implicit val typeScope: TypeScope = ImmutableTypeScope.from(typeParameters, parentScope)
    val supertypes = InheritanceResolver.resolveInheritedTypes(node.extended, TraitIllegalExtends(node))
    new TraitSchema(node.name, typeParameters, supertypes)
  }

}
