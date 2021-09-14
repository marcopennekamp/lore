package lore.compiler.resolution

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.StructSchema

object StructSchemaResolver {

  case class StructIllegalExtends(node: DeclNode.StructNode) extends Feedback.Error(node) {
    override def message = s"The struct ${node.name} does not implement a trait or shape but some other type."
  }

  def resolve(node: DeclNode.StructNode, parentScope: TypeScope)(implicit reporter: Reporter): StructSchema = {
    val typeParameters = TypeVariableResolver.resolve(node.typeVariables, parentScope)
    implicit val typeScope: TypeScope = ImmutableTypeScope.from(typeParameters, parentScope)
    val supertypes = InheritanceResolver.resolveInheritedTypes(node.extended, StructIllegalExtends(node))
    new StructSchema(node.name, typeParameters, supertypes)
  }

}
