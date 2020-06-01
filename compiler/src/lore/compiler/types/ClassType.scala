package lore.compiler.types

import lore.types
import lore.types.Type

class ClassType(override val schema: ClassTypeSchema, override val supertype: Option[ClassType]) extends lore.types.ClassType with DeclaredType {
  override def typeArguments: List[Type] = Nil
  override def ownedBy: Option[Type] = schema.ownedBy
  override def componentTypes: List[types.ComponentType] = schema.componentTypes
}
