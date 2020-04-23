package lore.compiler.feedback

import lore.ast._
import lore.compiler.Fragment
import lore.compiler.phases.resolution.FragmentNode
import lore.definitions.{FunctionDefinition, PositionedDefinition}

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
  // TODO: Consider moving more error definitions to their point of use if they're only used in one place.

  case class FunctionNotFound(name: String, node: ExprNode.CallNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"The function $name does not exist in the current scope."
  }

  case class TypeNotFound(name: String, pos: Position) extends Error(pos) {
    override def message = s"The type $name does not exist in the current scope."
  }

  case class ComponentTypeMustContainClass(node: TypeExprNode.ComponentNode)(implicit fragment: Fragment) extends Error(node) {
    override def message: String = s"The component type +${node.underlyingName} must contain a class type. ${node.underlyingName} is not a class."
  }

  case class ClassMustExtendClass(node: TypeDeclNode.ClassNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"The class ${node.name} does not extend a class but some other type."
  }

  case class ComponentMustBeClass(node: TypeDeclNode.ComponentNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"The component ${node.name} is not a valid class type."
  }

  case class LabelMustExtendLabel(node: TypeDeclNode.LabelNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"The label ${node.name} does not extend a label but some other type."
  }

  case class TypeAlreadyExists(node: TypeDeclNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"The type ${node.name} is already declared somewhere else."
  }

  case class MultiFunctionNotFound(name: String, pos: Position) extends Error(pos) {
    override def message = s"The multi-function $name does not exist in the current scope."
  }

  case class IllegalContinuation(node: TopLevelExprNode.ContinuationNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"A continuation is only valid as the very last top-level expression of a constructor block."
  }

  /**
    * @param occurrence One of the type declarations where the cycles occurs, so that we can report one error location.
    */
  case class InheritanceCycle(cycle: List[String], occurrence: FragmentNode[TypeDeclNode]) extends Error(occurrence) {
    override def message: String = s"""
    |An inheritance cycle between the following types has been detected: ${cycle.mkString(", ")}.
    |A class or label A cannot inherit from a class/label B that also inherits from A directly or indirectly. The
    |subtyping relationships of declared types must result in a directed, acyclic graph.
    """.stripMargin.replaceAll("\n", " ").trim
  }
}
