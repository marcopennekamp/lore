package lore.compiler.feedback

import com.typesafe.scalalogging.Logger
import lore.compiler.core.{Position, Positioned}

/**
  * Feedback is either an error or a warning.
  *
  * TODO (syntax): Rename to `Problem` so that there's a distinction between "problem" and "problems", as opposed to
  *                "feedback" and "feedback".
  */
sealed trait Feedback extends Positioned {
  /**
    * The message shown to the user once the feedback is logged.
    */
  def message: String

  /**
    * A stack trace of the instance where the feedback was initialized.
    *
    * TODO (syntax): Only collect stack traces if "error stack traces" are enabled.
    */
  val stackTrace: Vector[StackTraceElement] = new Throwable().getStackTrace.toVector

  /**
    * Whether this feedback is an error. The presence of error feedback will stop the compilation after program
    * analysis, and possibly earlier if early exit is enabled.
    */
  def isError: Boolean = false

  override def toString: String = Feedback.stringify(this)
}

object Feedback {

  /**
    * The base class for compilation warnings. Proper warnings must extend this class.
    */
  abstract class Warning(override val position: Position) extends Feedback

  /**
    * The base class for compilation errors. Proper errors must extend this class.
    */
  abstract class Error(override val position: Position) extends Feedback {
    def this(definition: Positioned) = this(definition.position)
    override def isError = true
  }

  val tagError = s"${Console.RED}error${Console.RESET}"
  val tagWarning = s"${Console.YELLOW}warning${Console.RESET}"

  /**
    * This logger is used to report errors and warnings to the user.
    */
  val logger: Logger = Logger("lore.compiler.feedback")
  val loggerBlank: Logger = Logger("lore.compiler.feedback.blank")

  /**
    * Sorts the given feedback such that feedback is ordered by fragments first and index second.
    */
  def sort(feedback: Vector[Feedback]): Vector[Feedback] = feedback.sortWith { case (f1, f2) => f1.position < f2.position }

  /**
    * Creates a pretty string representation of the given feedback.
    */
  def stringify(feedback: Feedback, showStackTraces: Boolean = false): String = {
    val stackTrace = if (showStackTraces) s"\n${feedback.stackTrace.mkString("\n")}" else ""
    val head = if (feedback.position != Position.unknown) s"${feedback.position}: " else ""
    s"$head${feedback.message}$stackTrace"
  }

  /**
    * Logs the given feedback using the Feedback logger.
    */
  def log(feedback: Feedback, showStackTraces: Boolean = false): Unit = {
    feedback match {
      case _: Warning => logger.warn(stringify(feedback, showStackTraces))
      case _: Error => logger.error(stringify(feedback, showStackTraces))
    }
  }

  /**
    * Sorts and logs the given list of feedback using the Feedback logger.
    */
  def logAll(feedback: Vector[Feedback], showStackTraces: Boolean = false): Unit = {
    sort(feedback).foreach(log(_, showStackTraces))
  }

}
