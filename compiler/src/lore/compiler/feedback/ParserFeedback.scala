package lore.compiler.feedback

import lore.compiler.core.Position

object ParserFeedback {
  object Declarations {
    case class ModuleExpected(override val position: Position) extends Feedback.Error(position) {
      override def message: String = "Expected a `module` keyword or `@root` annotation."
    }
  }
}
