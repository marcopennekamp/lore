package lore.compiler.syntax

import lore.compiler.core.{Fragment, Position, Positioned}
import lore.compiler.semantics.NamePath

trait Node extends Positioned

object Node {
  type Index = Int

  /**
    * A node that represents some other node's name. Name nodes have their own position. This makes them useful in
    * situations where we only want to consider a node's name, not the whole node. E.g. when reporting feedback for
    * declared types, in syntax highlighting, or when going to a definition.
    */
  case class NameNode(value: String, position: Position) extends Node

  /**
    * The name path version of [[NameNode]].
    */
  case class NamePathNode(segments: Vector[NameNode]) extends Node {
    val position: Position = if (segments.nonEmpty) segments.head.position.to(segments.last.position) else Position.unknown
    val simpleNameNode: NameNode = if (segments.nonEmpty) segments.last else NameNode("", position)
    val namePath: NamePath = NamePath.from(this)
  }

  object NamePathNode {
    def apply(segments: NameNode*): NamePathNode = NamePathNode(segments.toVector)

    val empty: NamePathNode = NamePathNode()
  }

  trait NamedNode extends Node {
    def nameNode: NameNode
    def name: String = nameNode.value
  }

  trait PathNamedNode extends Node {
    def namePathNode: NamePathNode
    def namePath: NamePath = namePathNode.namePath
  }

  /**
    * Construct a 1-element node with a position.
    */
  def withPosition[T1, R <: Node](construct: (T1, Position) => R)(args: (Index, T1, Index))(implicit fragment: Fragment): R = {
    val (startIndex, p1, endIndex) = args
    construct(p1, Position(fragment, startIndex, endIndex))
  }

  /**
    * Construct a 1-element node with a position.
    */
  def withPositionUntupled[T1, R <: Node](construct: (T1, Position) => R)(startIndex: Index, p1: T1, endIndex: Index)(implicit fragment: Fragment): R = {
    construct(p1, Position(fragment, startIndex, endIndex))
  }

  /**
    * Construct a 2-element node with a position.
    */
  def withPosition[T1, T2, R <: Node](construct: (T1, T2, Position) => R)(args: (Index, T1, T2, Index))(implicit fragment: Fragment): R = {
    val (startIndex, p1, p2, endIndex) = args
    construct(p1, p2, Position(fragment, startIndex, endIndex))
  }

  /**
    * Construct a 2-element node with a position.
    */
  def withPositionUntupled[T1, T2, R <: Node](construct: (T1, T2, Position) => R)(startIndex: Index, p1: T1, p2: T2, endIndex: Index)(implicit fragment: Fragment): R = {
    construct(p1, p2, Position(fragment, startIndex, endIndex))
  }

  /**
    * Construct a 3-element node with a position.
    */
  def withPosition[T1, T2, T3, R <: Node](construct: (T1, T2, T3, Position) => R)(args: (Index, T1, T2, T3, Index))(implicit fragment: Fragment): R = {
    val (startIndex, p1, p2, p3, endIndex) = args
    construct(p1, p2, p3, Position(fragment, startIndex, endIndex))
  }

  /**
    * Construct a 4-element node with a position.
    */
  def withPosition[T1, T2, T3, T4, R <: Node](construct: (T1, T2, T3, T4, Position) => R)(args: (Index, T1, T2, T3, T4, Index))(implicit fragment: Fragment): R = {
    val (startIndex, p1, p2, p3, p4, endIndex) = args
    construct(p1, p2, p3, p4, Position(fragment, startIndex, endIndex))
  }

  /**
    * Construct a 5-element node with a position.
    */
  def withPosition[T1, T2, T3, T4, T5, R <: Node](
    construct: (T1, T2, T3, T4, T5, Position) => R
  )(args: (Index, T1, T2, T3, T4, T5, Index))(implicit fragment: Fragment): R = {
    val (startIndex, p1, p2, p3, p4, p5, endIndex) = args
    construct(p1, p2, p3, p4, p5, Position(fragment, startIndex, endIndex))
  }

  /**
    * Construct a 6-element node with a position.
    */
  def withPosition[T1, T2, T3, T4, T5, T6, R <: Node](
    construct: (T1, T2, T3, T4, T5, T6, Position) => R
  )(args: (Index, T1, T2, T3, T4, T5, T6, Index))(implicit fragment: Fragment): R = {
    val (startIndex, p1, p2, p3, p4, p5, p6, endIndex) = args
    construct(p1, p2, p3, p4, p5, p6, Position(fragment, startIndex, endIndex))
  }

  /**
    * Construct a 7-element node with a position.
    */
  def withPosition[T1, T2, T3, T4, T5, T6, T7, R <: Node](
    construct: (T1, T2, T3, T4, T5, T6, T7, Position) => R
  )(args: (Index, T1, T2, T3, T4, T5, T6, T7, Index))(implicit fragment: Fragment): R = {
    val (startIndex, p1, p2, p3, p4, p5, p6, p7, endIndex) = args
    construct(p1, p2, p3, p4, p5, p6, p7, Position(fragment, startIndex, endIndex))
  }
}
