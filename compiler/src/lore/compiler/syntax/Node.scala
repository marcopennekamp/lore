package lore.compiler.syntax

import lore.compiler.core.{Fragment, Position}

trait Node {
  /**
    * The first position of the current node in the original source code.
    */
  def position: Position
}

object Node {
  type Index = Int

  /**
    * Construct a 1-element node with a position.
    */
  def withPosition[T1, R <: Node](construct: (T1, Position) => R)(args: (Index, T1))(implicit fragment: Fragment): R = {
    val (index, p1) = args
    construct(p1, Position(fragment, index))
  }

  /**
    * Construct a 1-element node with a position.
    */
  def withPositionUntupled[T1, R <: Node](construct: (T1, Position) => R)(index: Index, p1: T1)(implicit fragment: Fragment): R = {
    construct(p1, Position(fragment, index))
  }

  /**
    * Construct a 2-element node with a position.
    */
  def withPosition[T1, T2, R <: Node](construct: (T1, T2, Position) => R)(args: (Index, T1, T2))(implicit fragment: Fragment): R = {
    val (index, p1, p2) = args
    construct(p1, p2, Position(fragment, index))
  }

  /**
    * Construct a 2-element node with a position.
    */
  def withPositionUntupled[T1, T2, R <: Node](construct: (T1, T2, Position) => R)(index: Index, p1: T1, p2: T2)(implicit fragment: Fragment): R = {
    construct(p1, p2, Position(fragment, index))
  }

  /**
    * Construct a 3-element node with a position.
    */
  def withPosition[T1, T2, T3, R <: Node](construct: (T1, T2, T3, Position) => R)(args: (Index, T1, T2, T3))(implicit fragment: Fragment): R = {
    val (index, p1, p2, p3) = args
    construct(p1, p2, p3, Position(fragment, index))
  }

  /**
    * Construct a 4-element node with a position.
    */
  def withPosition[T1, T2, T3, T4, R <: Node](construct: (T1, T2, T3, T4, Position) => R)(args: (Index, T1, T2, T3, T4))(implicit fragment: Fragment): R = {
    val (index, p1, p2, p3, p4) = args
    construct(p1, p2, p3, p4, Position(fragment, index))
  }

  /**
    * Construct a 5-element node with a position.
    */
  def withPosition[T1, T2, T3, T4, T5, R <: Node](
    construct: (T1, T2, T3, T4, T5, Position) => R
  )(args: (Index, T1, T2, T3, T4, T5))(implicit fragment: Fragment): R = {
    val (index, p1, p2, p3, p4, p5) = args
    construct(p1, p2, p3, p4, p5, Position(fragment, index))
  }

  /**
    * Construct a 6-element node with a position.
    */
  def withPosition[T1, T2, T3, T4, T5, T6, R <: Node](
    construct: (T1, T2, T3, T4, T5, T6, Position) => R
  )(args: (Index, T1, T2, T3, T4, T5, T6))(implicit fragment: Fragment): R = {
    val (index, p1, p2, p3, p4, p5, p6) = args
    construct(p1, p2, p3, p4, p5, p6, Position(fragment, index))
  }

  /**
    * Construct a 7-element node with a position.
    */
  def withPosition[T1, T2, T3, T4, T5, T6, T7, R <: Node](
    construct: (T1, T2, T3, T4, T5, T6, T7, Position) => R
  )(args: (Index, T1, T2, T3, T4, T5, T6, T7))(implicit fragment: Fragment): R = {
    val (index, p1, p2, p3, p4, p5, p6, p7) = args
    construct(p1, p2, p3, p4, p5, p6, p7, Position(fragment, index))
  }
}
