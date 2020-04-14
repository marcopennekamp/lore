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
      throw new IllegalStateException(s"The feedback instance $this may not be associated with a fragment twice.")
    }
    this.fragment = fragment
  }

  /**
    * The position of the feedback instance within its associated fragment. Before you call [[position]], you must
    * first ensure that [[associate]] has been called.
    */
  lazy val position: Fragment.Position = {
    if (fragment == null) {
      throw new IllegalStateException(s"Can't create a position for the feedback instance $this without an associated fragment.")
    }
    Fragment.Position(fragment, index)
  }
}

/**
  * The base class for compilation errors. Proper errors must extend this class.
  */
abstract class Error(override val index: ast.Index) extends Feedback {
  override def isSevere = true

  def this(node: Node) {
    this(node.index)
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

object Feedback {
  class FunctionNotFound(name: String, node: ExprNode.CallNode) extends Error(node) {
    override def message = s"The function $name does not exist in the current scope."
  }

  class TypeNotFound(name: String, node: TypeExprNode) extends Error(node) {
    override def message = s"The type $name does not exist in the current scope."
  }

  class ClassMustExtendClass(node: TypeDeclNode.ClassNode) extends Error(node) {
    override def message = s"The class ${node.name} does not extend a class but some other type."
  }

  class LabelMustExtendLabel(node: TypeDeclNode.LabelNode) extends Error(node) {
    override def message = s"The label ${node.name} does not extend a label but some other type."
  }
}
