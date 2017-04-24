package lore.parser

import fastparse.WhitespaceApi
import lore.ast._

class FpExpressionParser() extends ExpressionParser[Expr] {
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  val boolExpr: P[BoolExpr] = {
    val bool: P[Bool] = P(StringIn("true", "false").!).map(v => Bool(v.toBoolean))
    P(bool)
  }

  val numExpr: P[NumExpr] = {
    val num: P[Num] = P(CharIn('0' to '9').rep(1).!.map(a => Num(a.toLong)))
    val mul: P[Mul] = P(num ~ (CharIn("*") ~/ num).rep(1)).map { case (f0, fs) => fs.tail.foldLeft(Mul(f0, fs.head)) { case (a, b) => Mul(a, b) } }
    val branch: P[Branch] = P("if" ~/ "(" ~ boolExpr ~ ")" ~ numExpr ~ "else" ~ numExpr).map { case (c, i, e) => Branch(c, i, e) }
    P(mul | num | branch)
  }

  val expr: P[Expr] = P((numExpr | boolExpr) ~ End)

  override def parseExpression(text: String): Expr = {
    expr.parse(text) match {
      case Parsed.Success(result, _) => result
      case Parsed.Failure(_, _, info) => sys.error("Parsing failure! " + info)
    }
  }
}
