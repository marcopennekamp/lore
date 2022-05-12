package lore.compiler.feedback

import lore.compiler.semantics.structures.AliasDefinition

object AliasFeedback {
  case class StructExpected(alias: AliasDefinition) extends Feedback.Error(alias) {
    override def message: String = s"The struct alias `${alias.name}` must alias a struct type."
  }
}
