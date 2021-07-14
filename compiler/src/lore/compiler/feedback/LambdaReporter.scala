package lore.compiler.feedback

class LambdaReporter(f: Feedback => Unit) extends Reporter {
  private var _hasErrors = false
  override def report(feedback: Feedback): Unit = this.synchronized {
    if (feedback.isError) _hasErrors = true
    f(feedback)
  }
  override def hasErrors: Boolean = this.synchronized(_hasErrors)
}
