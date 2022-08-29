package lore.compiler.constraints

import lore.compiler.feedback.{AliasFeedback, Reporter}
import lore.compiler.types.AliasSchema.AliasVariant
import lore.compiler.types.{AliasSchema, StructType}

object AliasConstraints {

  /**
    * Verifies:
    *   1. If the type alias is a struct/object alias, the underlying type is a struct/object type.
    *   2. If the type alias is an object alias, it must not have any type parameters.
    */
  def verify(alias: AliasSchema)(implicit reporter: Reporter): Unit = {
    verifyUnderlyingStructType(alias)
    verifyConstantObjectAlias(alias)
  }

  /**
    * Verifies that the type alias has an underlying struct/object type if it is a struct/object alias.
    */
  private def verifyUnderlyingStructType(alias: AliasSchema)(implicit reporter: Reporter): Unit = {
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

  /**
    * Verifies that the object type alias has no type parameters.
    */
  private def verifyConstantObjectAlias(alias: AliasSchema)(implicit reporter: Reporter): Unit = {
    if (alias.isObjectAlias && alias.typeParameters.nonEmpty) {
      reporter.error(AliasFeedback.ConstantObjectAliasExpected(alias))
    }
  }

}
