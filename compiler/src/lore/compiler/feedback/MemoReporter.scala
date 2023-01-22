package lore.compiler.feedback

/**
  * A reporter that collects all feedback in a vector.
  */
class MemoReporter() extends Reporter {

  private var collectedFeedback: Vector[Feedback] = Vector.empty

  override def report(feedback: Feedback): Unit = this.synchronized {
    collectedFeedback = collectedFeedback :+ feedback
  }

  def feedback: Vector[Feedback] = collectedFeedback

  override def hasErrors: Boolean = this.synchronized(collectedFeedback.exists(_.isError))

  /**
    * Returns a copy of the currently collected feedback to save for [[restoreState]].
    */
  def currentState(): Vector[Feedback] = collectedFeedback

  /**
    * Restores the currently collected feedback to the given `feedback`, supporting backtracking.
    */
  def restoreState(feedback: Vector[Feedback]): Unit = this.synchronized {
    this.collectedFeedback = feedback
  }

}

object MemoReporter {

  def apply(): MemoReporter = new MemoReporter()

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
  def chain(parent: Reporter)(functions: Reporter => Unit*): Unit = {
    nested(parent) { reporter =>
      functions.foreach(f => if (!reporter.hasErrors) f(reporter))
    }
  }

}
