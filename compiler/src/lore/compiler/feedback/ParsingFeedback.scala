package lore.compiler.feedback

import lore.compiler.core.Position

object ParsingFeedback {
  case class ParseError(fastparseError: String, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The file had parse errors: $fastparseError"
  }
}
