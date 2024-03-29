package lore.compiler.types

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.definitions.TypeDefinition
import lore.compiler.syntax.DeclNode.AliasNode
import lore.compiler.types.AliasSchema.AliasVariant
import lore.compiler.types.TypeVariable.Assignments
import lore.compiler.utils.Once

class AliasSchema(
  override val name: NamePath,
  val aliasVariant: AliasVariant,
  override val node: AliasNode,
) extends TypeDefinition {
  private val _typeParameters: Once[Vector[TypeVariable]] = new Once
  private val _originalType: Once[Type] = new Once

  override def typeParameters: Vector[TypeVariable] = _typeParameters
  def originalType: Type = _originalType

  def initialize(typeParameters: Vector[TypeVariable], originalType: Type): Unit = {
    _typeParameters.assign(typeParameters)
    _originalType.assign(originalType)
  }

  override def isInitialized: Boolean = _typeParameters.isAssigned && _originalType.isAssigned

  override def instantiate(assignments: Assignments): Type = Type.substitute(originalType, assignments)

  def isStructAlias: Boolean = aliasVariant.isStructAlias
  def isObjectAlias: Boolean = aliasVariant == AliasVariant.Object

  override def position: Position = node.position
}

object AliasSchema {
  sealed trait AliasVariant {
    def isStructAlias: Boolean = this == AliasVariant.Struct || this == AliasVariant.Object
  }

  object AliasVariant {
    case object Type extends AliasVariant
    case object Struct extends AliasVariant
    case object Object extends AliasVariant
  }
}
