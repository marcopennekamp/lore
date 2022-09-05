package lore.compiler.types

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.syntax.DeclNode.TraitNode

class TraitSchema(
  override val name: NamePath,
  override val node: TraitNode,
) extends DeclaredSchema {
  override def kind: Kind = Kind.Trait
  override def constantType: TraitType = super.constantType.asInstanceOf[TraitType]
  override def identityType: TraitType = super.identityType.asInstanceOf[TraitType]
  override def instantiate(assignments: TypeVariable.Assignments): TraitType = TraitType(this, assignments)

  override def position: Position = node.position
}
