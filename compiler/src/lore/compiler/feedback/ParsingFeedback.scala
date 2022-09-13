package lore.compiler.feedback

import lore.compiler.core.Positioned

object ParsingFeedback {
  case class ParseError(fastparseError: String, positioned: Positioned) extends Feedback.Error(positioned) {
    override def message: String = s"The file had parse errors: $fastparseError"
  }
}
