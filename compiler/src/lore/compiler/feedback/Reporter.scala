package lore.compiler.feedback

/**
  * A reporter handles feedback raised during compilation. The different phases and subcomponents of the compiler are
  * usually invoked with a [[MemoReporter]], which collects all feedback. At crucial boundaries the compiler checks
  * that no errors have been produced, and only then can compilation continue. One such boundary is between the
  * transformation phase and the assembly phase: assembly can only be performed if no errors were found in previous
  * phases.
  *
  * Once compilation is complete, other reporters can be used to print out feedback, write it to a file, or do
  * something else entirely.
  *
  * Reporters must be thread-safe, as certain compilation operations are parallelized.
  */
trait Reporter {

  def report(feedback: Feedback): Unit
  def report(feedback: Vector[Feedback]): Unit = feedback.foreach(report)

  def error(feedback: Feedback.Error): Unit = report(feedback)
  def error(feedback: Vector[Feedback.Error]): Unit = report(feedback)

  def warn(feedback: Feedback.Warning): Unit = report(feedback)
  def warn(feedback: Vector[Feedback.Warning]): Unit = report(feedback)

  /**
    * Whether the reporter encountered any errors during its lifetime.
    */
  def hasErrors: Boolean

}
