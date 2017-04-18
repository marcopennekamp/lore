package lore.parser

import fastparse.WhitespaceApi
import lore.algebra.Expr
import matryoshka._
import matryoshka.implicits._

class FpExpressionParser[T]()(implicit T: Corecursive.Aux[T, Expr]) extends ExpressionParser[T] {
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  val num: P[T] = P(CharIn('0' to '9').rep(1).!.map(a => Expr.Num[T](a.toLong).embed))
  val mul: P[T] = P(num ~ "*" ~/ (mul | num)).map { case (n, e) => Expr.Mul[T](n, e).embed }
  val expr: P[T] = P((mul | num) ~ End)

  override def parseExpression(text: String): T = {
    expr.parse(text) match {
      case Parsed.Success(result, _) => result
      case Parsed.Failure(_, _, info) => println("Parsing failure! " + info); ???
    }
  }
}
