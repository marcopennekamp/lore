package lore.compiler.core

import lore.compiler.CompilerOptions

object FeedbackPrinter {
  val tagError = s"[${Console.RED}error${Console.RESET}]"
  val tagWarning = s"[${Console.YELLOW}warning${Console.RESET}]"
  val tagSuccess = s"[${Console.GREEN}success${Console.RESET}]"

  /**
    * Prints a list of feedback to a string.
    */
  def print(feedback: Vector[Feedback])(implicit options: CompilerOptions): String = printWithOptions(feedback, options.showFeedbackStackTraces)

  /**
    * Prints a list of feedback to a string.
    */
  def printWithOptions(feedback: Vector[Feedback], showFeedbackStackTraces: Boolean = false): String = {
    // Sort feedback such that instances are ordered by fragments first and index second.
    val sorted = feedback.sortWith { case (f1, f2) => f1.position < f2.position }

    // Now print each error with the proper index.
    sorted.map { feedback =>
      val message = s"${feedback.consoleTag} ${feedback.position}: ${feedback.message}"
      if (showFeedbackStackTraces) {
        s"""$message
           |${feedback.stackTrace.mkString("\n")}""".stripMargin
      } else {
        message
      }
    }.mkString("\n")
  }
}
