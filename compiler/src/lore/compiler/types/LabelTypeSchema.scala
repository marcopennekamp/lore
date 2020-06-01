package lore.compiler.types

import lore.compiler.definitions.LabelDefinition
import lore.types.Type

// TODO: Do we even WANT label types to be in a hierarchical relationship to each other?
class LabelTypeSchema(
  override val superschema: Option[LabelTypeSchema],
) extends lore.types.LabelTypeSchema with DeclaredTypeSchema with DeclaredTypeSchema.DefinitionProperty[LabelDefinition] {
  override def rootSuperschema: LabelTypeSchema = super.rootSuperschema.asInstanceOf[LabelTypeSchema]
  override def typeParameters: List[TypeVariable] = Nil

  override def instantiate(types: List[Type]): LabelType = {
    if (types.nonEmpty) {
      throw new RuntimeException("Label types can't have type arguments. This is a compiler bug!")
    }

    val supertype = superschema.map(_.instantiate(types))
    new LabelType(this, supertype)
  }
}
