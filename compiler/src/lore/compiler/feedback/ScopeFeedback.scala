package lore.compiler.feedback

import lore.compiler.core.{Position, Positioned}

object ScopeFeedback {
  case class AlreadyDeclared(label: String, name: String, positioned: Positioned) extends Feedback.Error(positioned) {
    override def message = s"The $label `$name` has already been declared in the current scope."
  }

  case class UnknownEntry(label: String, name: String, positioned: Positioned) extends Feedback.Error(positioned) {
    override def message = s"The $label `$name` does not exist in the current scope."
  }

  case class ModuleNotFound(name: String, positioned: Positioned) extends Feedback.Error(positioned) {
    override def message = s"The binding `$name` is not a module or does not exist in the current scope."
  }
}
