package lore.types

import lore.definitions.DataTypeDefinition
import lore.execution.Context

trait DataType extends DeclaredType {
  def supertype: Option[DataType]
  def isAbstract: Boolean
  override def definition: DataTypeDefinition

  /**
    * The name of the kind of type, e.g. 'class' or 'entity'.
    */
  def kindName: String

  override def directDeclaredSubtypes(implicit context: Context): Set[Type] = {
    context.types.values.flatMap {
      case t: DataType if t.supertype.contains(this) => Some(t)
      case _ => None
    }.toSet
  }
  override def verbose = s"${if (isAbstract) s"abstract $kindName" else kindName} $toString < ${supertype.getOrElse(AnyType)}"
}
