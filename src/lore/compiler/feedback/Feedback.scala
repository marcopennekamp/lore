package lore.compiler.feedback

import lore.ast._
import lore.compiler.Fragment
import lore.compiler.phases.resolution.FragmentNode
import lore.definitions.PositionedDefinition

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
  override def isSevere = true
  override val consoleTag: String = FeedbackPrinter.tagError

  def this(node: Node)(implicit fragment: Fragment) {
    this(node.position)
  }

  def this(fragmentNode: FragmentNode[Node]) {
    this(fragmentNode.position)
  }

  def this(definition: PositionedDefinition) {
    this(definition.position)
  }
}

object Error {
  case class FunctionNotFound(name: String, node: ExprNode.SimpleCallNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"The function $name does not exist in the current scope."
  }
}
