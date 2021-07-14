package lore.compiler.syntax.visitor

import lore.compiler.core.Position
import lore.compiler.syntax._
import scalaz.Id.Id

/**
  * Searches for the first [[Node]] that encompasses the target position and for which the `result` function returns a
  * `Some`. The first node is defined by a depth-first ordering.
  */
abstract class NodeSeeker[A](target: NodeSeeker.Target) extends CombiningNodeVisitor[Option[A], Id] {

  /**
    * Handle the node found at the target position, returning `Some` with a result value if it is an acceptable node.
    */
  protected def result(node: Node): Option[A]

  override protected def visit(node: Node, innerResult: Option[A]): Option[A] = innerResult.orElse(check(node))

  private def check(node: Node): Option[A] = {
    if (target.containedIn(node.position)) result(node) else None
  }

}

object NodeSeeker {

  case class Target(line: Int, column: Int) {
    def containedIn(position: Position): Boolean = containedIn(position.startLine, position.startColumn, position.endLine, position.endColumn)

    def containedIn(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Boolean = {
      (startLine < this.line || startLine == this.line && startColumn <= this.column) &&
        (this.line < endLine || endLine == this.line && this.column <= endColumn)
    }
  }

  def visit[A](seeker: NodeSeeker[A])(node: Node): Option[A] = CombiningNodeVisitor.OptionApplicator(seeker).visit(node)

}
