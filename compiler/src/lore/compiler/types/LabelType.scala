package lore.compiler.types
import lore.types.Type

class LabelType(override val schema: LabelTypeSchema, override val supertype: Option[LabelType]) extends lore.types.LabelType with DeclaredType {
  override def typeArguments: List[Type] = Nil
}
