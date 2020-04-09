package lore

import fastparse._
import lore.ast.ExprNode

import ScalaWhitespace._

package object parser {
  /**
    * A helper parser that parses binary operators.
    */
  def binary[Operand, Result, _: P](op: => P[Unit], part: => P[Operand], node: (Operand, Operand) => Result): P[Result] = {
    P(part ~ op ~ part).map(node.tupled)
  }

  /**
    * A helper parser that parses an arbitrary number of operands connected by a single operator, list-style.
    */
  def xaryList[Operand, Result, _: P](op: => P[Unit], part: => P[Operand], node: List[Operand] => Result): P[Result] = {
    P(part ~ (op ~ part).rep(1)).map { case (e, es) => node(e +: es.toList) }
  }

  /**
    * A helper parser that parses an arbitrary number of operands connected by a single operator, set-style.
    */
  def xarySet[Operand, Result, _: P](op: => P[Unit], part: => P[Operand], node: Set[Operand] => Result): P[Result] = {
    P(part ~ (op ~ part).rep(1)).map { case (e, es) => node(es.toSet + e) }
  }
}
