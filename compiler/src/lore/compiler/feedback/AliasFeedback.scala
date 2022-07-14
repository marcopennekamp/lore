package lore.compiler.feedback

import lore.compiler.types.AliasSchema

object AliasFeedback {

  case class StructExpected(alias: AliasSchema) extends Feedback.Error(alias) {
    override def message: String = s"The struct alias `${alias.simpleName}` must alias a non-object struct type."
  }

  case class ObjectExpected(alias: AliasSchema) extends Feedback.Error(alias) {
    override def message: String = s"The object alias `${alias.simpleName}` must alias an object type."
  }

  case class ConstantObjectAliasExpected(alias: AliasSchema) extends Feedback.Error(alias) {
    override def message: String = s"The object alias `${alias.simpleName}` must not have any type parameters."
  }

}
