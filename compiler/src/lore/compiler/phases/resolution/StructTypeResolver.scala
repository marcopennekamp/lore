package lore.compiler.phases.resolution

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types.StructType

object StructTypeResolver {

  case class StructIllegalExtends(node: TypeDeclNode.StructNode) extends Feedback.Error(node) {
    override def message = s"The struct ${node.name} does not implement a trait or shape but some other type."
  }

  def resolve(node: TypeDeclNode.StructNode)(implicit typeScope: TypeScope, reporter: Reporter): StructType = {
    val supertypes = InheritanceResolver.resolveInheritedTypes(node.extended, StructIllegalExtends(node))
    new StructType(node.name, supertypes)
  }

}
