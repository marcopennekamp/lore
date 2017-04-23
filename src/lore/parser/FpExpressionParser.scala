package lore.parser

import fastparse.WhitespaceApi
import lore.ast.{Expr, Mul, Num}

class FpExpressionParser() extends ExpressionParser[Expr] {
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  val num: P[Num] = P(CharIn('0' to '9').rep(1).!.map(a => Num(a.toLong)))
  val mul: P[Mul] = P(num ~ "*" ~/ (mul | num)).map { case (n, e) => Mul(n, e) }
  val expr: P[Expr] = P((mul | num) ~ End)

  override def parseExpression(text: String): Expr = {
    expr.parse(text) match {
      case Parsed.Success(result, _) => result
      case Parsed.Failure(_, _, info) => println("Parsing failure! " + info); ???
    }
  }
}
