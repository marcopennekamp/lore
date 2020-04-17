package lore.compiler

object FeedbackPrinter {
  /**
    * Prints a list of feedback to a string.
    */
  def print(feedback: List[Feedback]): String = {
    // Sort feedback such that instances are ordered by fragments first and index second.
    val sorted = feedback.sortWith { case (f1, f2) => f1.position < f2.position }

    // Now print each error with the proper index.
    sorted.map { feedback =>
      s"[${feedback.kind}] ${feedback.position.fragment.name} at ${feedback.position.prettyIndex}:\n${feedback.message}"
    }.mkString("\n")
  }
}
