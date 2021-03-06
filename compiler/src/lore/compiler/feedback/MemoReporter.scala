package lore.compiler.feedback

/**
  * A reporter that collects all feedback in a vector.
  */
class MemoReporter(initialFeedback: Vector[Feedback]) extends Reporter {

  private val state = new MemoReporter.State(initialFeedback)

  override def report(feedback: Feedback): Unit = this.synchronized {
    if (feedback.isError) {
      state.hasErrors = true
    }
    state.feedback = state.feedback :+ feedback
  }

  override def hasErrors: Boolean = this.synchronized(state.hasErrors)

  def feedback: Vector[Feedback] = this.synchronized(state.feedback)

}

object MemoReporter {

  private class State(initialFeedback: Vector[Feedback]) {
    var feedback: Vector[Feedback] = initialFeedback
    var hasErrors: Boolean = initialFeedback.exists(_.isError)
  }

  def apply(initialFeedback: Vector[Feedback]): MemoReporter = new MemoReporter(initialFeedback)
  def apply(): MemoReporter = apply(Vector.empty)

  /**
    * Invokes `f` with a fresh MemoReporter, then adds the feedback to the given parent reporter.
    */
  def nested[A](parent: Reporter)(f: MemoReporter => A): A = {
    val reporter = MemoReporter()
    val result = f(reporter)
    parent.report(reporter.feedback)
    result
  }

  /**
    * Invokes the first function with a fresh MemoReporter, then any subsequent function with the same reporter, but
    * only as long as no errors have been reported. All feedback is forwarded to the given parent reporter.
    */
  def chain[A, B](parent: Reporter)(functions: Reporter => Unit*): Unit = {
    nested(parent) { reporter =>
      functions.foreach(f => if (!reporter.hasErrors) f(reporter))
    }
  }

}
