package lore.compiler.phases.resolution

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types.StructSchema

object StructSchemaResolver {

  case class StructIllegalExtends(node: TypeDeclNode.StructNode) extends Feedback.Error(node) {
    override def message = s"The struct ${node.name} does not implement a trait or shape but some other type."
  }

  def resolve(node: TypeDeclNode.StructNode, parentScope: TypeScope)(implicit reporter: Reporter): StructSchema = {
    val typeParameters = TypeVariableDeclarationResolver.resolve(node.typeVariables, parentScope)
    implicit val typeScope: TypeScope = ImmutableTypeScope.from(typeParameters, parentScope)
    val supertypes = InheritanceResolver.resolveInheritedTypes(node.extended, StructIllegalExtends(node))
    new StructSchema(node.name, typeParameters, supertypes)
  }

}
