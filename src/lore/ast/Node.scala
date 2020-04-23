package lore.ast

import lore.ast.Node.Index
import lore.compiler.Fragment
import lore.compiler.feedback.Position

trait Node {
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
