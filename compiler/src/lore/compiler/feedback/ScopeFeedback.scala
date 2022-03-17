package lore.compiler.feedback

import lore.compiler.core.Position

object ScopeFeedback {
  case class AlreadyDeclared(label: String, name: String, override val position: Position) extends Feedback.Error(position) {
    override def message = s"The $label `$name` has already been declared in the current scope."
  }

  case class UnknownEntry(label: String, name: String, override val position: Position) extends Feedback.Error(position) {
    override def message = s"The $label `$name` does not exist in the current scope."
  }

  case class ModuleNotFound(name: String, override val position: Position) extends Feedback.Error(position) {
    override def message = s"The binding `$name` is not a module or does not exist in the current scope."
  }

  case class ModuleExpected(name: String, override val position: Position) extends Feedback.Error(position) {
    override def message = s"The binding `$name` must be a module."
  }
}
