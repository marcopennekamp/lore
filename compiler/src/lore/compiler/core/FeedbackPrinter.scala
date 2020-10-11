package lore.compiler.core

object FeedbackPrinter {
  val tagError = s"[${Console.RED}error${Console.RESET}]"
  val tagWarning = s"[${Console.YELLOW}warning${Console.RESET}]"
  val tagSuccess = s"[${Console.GREEN}success${Console.RESET}]"

  /**
    * Prints a list of feedback to a string.
    */
  def print(feedback: Vector[Feedback]): String = {
    // Sort feedback such that instances are ordered by fragments first and index second.
    val sorted = feedback.sortWith { case (f1, f2) => f1.position < f2.position }

    // Now print each error with the proper index.
    sorted.map { feedback =>
      // TODO: Add a feature toggle to switch stack traces in reporting on and off.
      val showStackTraces = false
      val message = s"${feedback.consoleTag} ${feedback.position}: ${feedback.message}"
      if (showStackTraces) {
        s"""$message
           |${feedback.stackTrace.mkString("\n")}""".stripMargin
      } else {
        message
      }
    }.mkString("\n")
  }
}
