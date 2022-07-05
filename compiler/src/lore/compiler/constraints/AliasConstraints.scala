package lore.compiler.constraints

import lore.compiler.feedback.{AliasFeedback, Reporter}
import lore.compiler.types.AliasSchema.AliasVariant
import lore.compiler.types.{AliasSchema, StructType}

object AliasConstraints {

  /**
    * Verifies that, if the type alias is a struct/object alias, the underlying type is a struct/object type.
    */
  def verify(alias: AliasSchema)(implicit reporter: Reporter): Unit = {
    alias.aliasVariant match {
      case AliasVariant.Struct => alias.originalType match {
        case tpe: StructType if !tpe.schema.isObject =>
        case _ => reporter.error(AliasFeedback.StructExpected(alias))
      }

      case AliasVariant.Object => alias.originalType match {
        case tpe: StructType if tpe.schema.isObject =>
        case _ => reporter.error(AliasFeedback.ObjectExpected(alias))
      }

      case AliasVariant.Type =>
    }
  }

}
