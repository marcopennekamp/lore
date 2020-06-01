package lore.compiler.types

import lore.compiler.definitions.ClassDefinition
import lore.types.Type

class ClassTypeSchema(
  override val superschema: Option[ClassTypeSchema], val ownedByDeferred: Option[OwnedByDeferred], val isAbstract: Boolean
) extends lore.types.ClassTypeSchema with DeclaredTypeSchema with DeclaredTypeSchema.DefinitionProperty[ClassDefinition] {
  override def ownedBy: Option[Type] = ownedByDeferred.map(_.tpe)
  override def isEntity: Boolean = this.definition.isEntity
  override lazy val componentTypes: List[ComponentType] = this.definition.components.map(_.componentType)
  override def rootSuperschema: ClassTypeSchema = super.rootSuperschema.asInstanceOf[ClassTypeSchema]

  override def typeParameters: List[TypeVariable] = Nil
  override def instantiate(types: List[Type]): ClassType = {
    if (types.nonEmpty) {
      throw new RuntimeException("Class types can't have type arguments YET. This is a compiler bug!")
    }

    val supertype = superschema.map(_.instantiate(types))
    new ClassType(this, supertype)
  }
}
