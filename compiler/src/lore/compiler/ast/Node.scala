package lore.compiler.ast

import lore.compiler.ast.Node.Index
import lore.compiler.core.Fragment
import lore.compiler.feedback.Position

trait Node {
  // TODO: We could turn nodes into an algebraic data type where we substitute node children with arbitrary
  //       types. This would allow us to define a statement visitor without the mass of ugly visit methods.
  //       We could simply have one visit method on which we match the original tree input IfELseNode[StmtNode]
  //       and the output IfElseNode[String] (in case of transpilation, for example).

  /**
    * The start index of the current node in the original source code.
    */
  var index: Index = 0

  /**
    * Creates a fragment position from the node's index and the given fragment.
    */
  def position(implicit fragment: Fragment): Position = Position(fragment, index)
}

object Node {
  type Index = Int

  // TODO: Reimplement withIndex using shapeless.

  private def withIndexImpl[T, R <: Node](construct: T => R, index: Index, t: T): R = {
    val node = construct(t)
    node.index = index
    node
  }

  /**
    * Construct a 1-element node and set its index as given by the parser.
    */
  def withIndex[T1, R <: Node](construct: T1 => R)(args: (Index, T1)): R = {
    val (index, p1) = args
    withIndexImpl(construct, index, p1)
  }

  /**
    * Construct a 2-element node and set its index as given by the parser.
    */
  def withIndex[T1, T2, R <: Node](construct: (T1, T2) => R)(args: (Index, T1, T2)): R = {
    val (index, p1, p2) = args
    withIndexImpl(construct.tupled, index, (p1, p2))
  }

  /**
    * Construct a 3-element node and set its index as given by the parser.
    */
  def withIndex[T1, T2, T3, R <: Node](construct: (T1, T2, T3) => R)(args: (Index, T1, T2, T3)): R = {
    val (index, p1, p2, p3) = args
    withIndexImpl(construct.tupled, index, (p1, p2, p3))
  }
}
