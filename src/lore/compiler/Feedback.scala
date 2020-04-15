package lore.compiler

import lore.ast
import lore.ast.{ExprNode, Node, TypeDeclNode, TypeExprNode}

/**
  * A Feedback instance is a compiler report. The most notable Feedback types are warnings and errors.
  * Feedback instances are always associated with a specific position in a fragment (the index given by
  * the parser). The fragment is associated with when feedback is aggregated.
  */
sealed trait Feedback {
  def index: ast.Index
  def message: String

  /**
    * Whether the feedback necessitates the termination of compilation.
    */
  def isSevere: Boolean

  private var fragment: Fragment = _

  /**
    * Associates the feedback object with the given fragment.
    */
  def associate(fragment: Fragment): Unit = {
    if (this.fragment != null) {
      println("An attempt to associate an already associated feedback instance with a fragment was blocked.")
      // We only throw an exception if the feedback would be associated with two different fragment instances.
      if (this.fragment != fragment) {
        throw new IllegalStateException(s"A feedback instance $this may not be associated with two different fragments.")
      }
    } else {
      this.fragment = fragment
    }
  }

  /**
    * The position of the feedback instance within its associated fragment. Before you call [[position]], you must
    * first ensure that [[associate]] has been called.
    */
  lazy val position: Position = {
    if (fragment == null) {
      throw new IllegalStateException(s"Can't create a position for the feedback instance $this without an associated fragment.")
    }
    Position(fragment, index)
  }
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
abstract class Warning(override val index: ast.Index) extends InfoFeedback

/**
  * The base class for compilation errors. Proper errors must extend this class.
  */
abstract class Error(override val index: ast.Index) extends Feedback {
  override def isSevere = true

  def this(node: Node) {
    this(node.index)
  }

  def this(fragmentNode: FragmentNode[Node]) {
    this(fragmentNode.node.index)
    associate(fragmentNode.fragment)
  }
}

object Error {
  case class FunctionNotFound(name: String, node: ExprNode.CallNode) extends Error(node) {
    override def message = s"The function $name does not exist in the current scope."
  }

  case class TypeNotFound(name: String, node: Node) extends Error(node) {
    override def message = s"The type $name does not exist in the current scope."
  }

  case class ComponentTypeMustContainClass(node: TypeExprNode.ComponentNode) extends Error(node) {
    override def message: String = s"The component type +${node.underlyingName} must contain a class type. ${node.underlyingName} is not a class."
  }

  case class ClassMustExtendClass(node: TypeDeclNode.ClassNode) extends Error(node) {
    override def message = s"The class ${node.name} does not extend a class but some other type."
  }

  case class ComponentMustBeClass(node: TypeDeclNode.ComponentNode) extends Error(node) {
    override def message = s"The component ${node.name} is not a valid class type."
  }

  case class LabelMustExtendLabel(node: TypeDeclNode.LabelNode) extends Error(node) {
    override def message = s"The label ${node.name} does not extend a label but some other type."
  }

  case class TypeAlreadyExists(node: TypeDeclNode) extends Error(node) {
    override def message = s"The type ${node.name} is already declared somewhere else."
  }

  /**
    * @param occurrence One of the type declarations where the cycles occurs, so that we can report one error location.
    */
  case class InheritanceCycle(cycle: List[String], occurrence: FragmentNode[TypeDeclNode]) extends Error(occurrence) {
    override def message: String = """
    |An inheritance cycle between the following types has been detected: ${cycle.mkString(",")}.
    |A class or label A cannot inherit from a class/label B that also inherits from A directly or indirectly. The
    |subtyping relationships of declared types must result in a directed, acyclic graph.
    """.stripMargin.replaceAll("\n", " ").trim
  }
}
