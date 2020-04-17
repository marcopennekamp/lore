package lore.compiler

object FeedbackPrinter {
  val tagError = s"[${Console.RED}error${Console.RESET}]"
  val tagWarning = s"[${Console.YELLOW}warning${Console.RESET}]"
  val tagSuccess = s"[${Console.GREEN}success${Console.RESET}]"

  /**
    * Prints a list of feedback to a string.
    */
  def print(feedback: List[Feedback]): String = {
    // Sort feedback such that instances are ordered by fragments first and index second.
    val sorted = feedback.sortWith { case (f1, f2) => f1.position < f2.position }

    // Now print each error with the proper index.
    sorted.map { feedback =>
      s"${feedback.consoleTag} ${feedback.position.fragment.name} (${feedback.position.prettyIndex}): ${feedback.message}"
    }.mkString("\n")
  }
}
