package lore.compiler.feedback

import lore.compiler.semantics.specs.SpecDefinition

object SpecFeedback {
  case class NameTaken(spec: SpecDefinition) extends Feedback.Error(spec) {
    override def message: String = s"The name of the spec `${spec.name}` is already taken by another spec."
  }

  case class AlreadyExists(spec: SpecDefinition) extends Feedback.Error(spec) {
    override def message: String = s"The spec `${spec.name}` is already declared somewhere else."
  }
}
