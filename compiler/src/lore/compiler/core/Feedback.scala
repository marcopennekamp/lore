package lore.compiler.core

import lore.compiler.semantics.expressions.Expression
import lore.compiler.syntax._

/**
  * A Feedback instance is a compiler report. The most notable Feedback types are warnings and errors.
  * Feedback instances are always associated with a specific [[Position]].
  */
sealed trait Feedback {
  def position: Position
  def message: String

  /**
    * The kind of feedback, to appear with the error message, such as warning or error. Possibly colored.
    */
  def consoleTag: String

  /**
    * Whether the feedback necessitates the termination of compilation.
    */
  def isSevere: Boolean

  /**
    * A stack trace of the instance where the feedback was initialized.
    */
  val stackTrace: Vector[StackTraceElement] = new Throwable().getStackTrace.toVector
}

/**
  * Any Feedback OTHER than Error which is supposed to be merely informative and not severe.
  */
trait InfoFeedback extends Feedback {
  override def isSevere = false
}

/**
  * The base class for compilation warnings. Proper warnings must extend this class.
  */
abstract class Warning(override val position: Position) extends InfoFeedback {
  override val consoleTag: String = FeedbackPrinter.tagWarning
}

/**
  * The base class for compilation errors. Proper errors must extend this class.
  */
abstract class Error(override val position: Position) extends Feedback {
  // TODO: We should add a "name" concept to an error so that programmers can quickly google/search for Lore errors.

  override def isSevere = true
  override val consoleTag: String = FeedbackPrinter.tagError

  def this(node: Node) = {
    this(node.position)
  }

  def this(definition: Positioned) = {
    this(definition.position)
  }

  def this(expression: Expression) = {
    this(expression.position)
  }
}
