package lore.compiler.phases.resolution

import lore.compiler.core.Compilation
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types.TraitType

object TraitTypeResolver {

  case class TraitIllegalExtends(node: TypeDeclNode.TraitNode) extends Feedback.Error(node) {
    override def message = s"The trait ${node.name} does not extend a trait or shape but some other type."
  }

  def resolve(node: TypeDeclNode.TraitNode)(implicit typeScope: TypeScope): Compilation[TraitType] = {
    InheritanceResolver.resolveInheritedTypes(node.extended, TraitIllegalExtends(node)).map {
      supertypes => new TraitType(node.name, supertypes)
    }
  }

}
