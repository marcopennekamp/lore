package lore.parser

import lore.algebra._
import matryoshka._
import matryoshka.implicits._

import scala.util.parsing.combinator.RegexParsers

class SpcExpressionParser extends RegexParsers {
  def EOF = "\\Z".r

  def num[T](implicit T: Corecursive.Aux[T, Expr]): Parser[T] =
    """(0|[1-9]\d*)""".r ^^ { a => Num[T](a.toInt).embed }
  def mul[T](implicit T: Corecursive.Aux[T, Expr]): Parser[T] =
    (num ~ "*" ~  mul | num ~ "*" ~ num) ^^ { case left ~ _ ~ right => Mul[T](left, right).embed }
  def expr[T](implicit T: Corecursive.Aux[T, Expr]): Parser[T] = (mul | num) ~ EOF ^^ { case e ~ _ => e }


  def parseExpression[T](text: String)(implicit T: Corecursive.Aux[T, Expr]): T = {
    parse(expr[T], text) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); ???
      case Error(msg, _) => println("ERROR: " + msg); ???
    }
  }
}
