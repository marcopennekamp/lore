package lore.parser

import lore.algebra.Expr.{Mul, Num}
import lore.algebra._
import matryoshka._
import matryoshka.implicits._

import scala.util.parsing.combinator.RegexParsers

class SpcExpressionParser[T](implicit T: Corecursive.Aux[T, Expr]) extends RegexParsers with ExpressionParser[T] {
  def EOF = "\\Z".r
  def num: Parser[T] = """(0|[1-9]\d*)""".r ^^ { a => Num[T](a.toInt).embed }
  def mul: Parser[T] = (num ~ "*" ~  mul | num ~ "*" ~ num) ^^ { case left ~ _ ~ right => Mul[T](left, right).embed }
  def expr: Parser[T] = (mul | num) ~ EOF ^^ { case e ~ _ => e }

  override def parseExpression(text: String): T = {
    parse(expr, text) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); ???
      case Error(msg, _) => println("ERROR: " + msg); ???
    }
  }
}
