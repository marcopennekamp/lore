package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

object ExpressionParser {

  def boolExpr[_ : P]: P[BoolExpr] = {
    def bool: P[Bool] = P(StringIn("true", "false").!).map(v => Bool(v.toBoolean))
    P(bool)
  }

  def numExpr[_ : P]: P[NumExpr] = {
    // TODO: Move literals to a NoWhitespace context.
    def num: P[Num] = P(CharIn("0-9").rep(1).!.map(a => Num(a.toLong)))
    def mul: P[Mul] = P(num ~ (CharIn("*") ~/ num).rep(1)).map { case (f0, fs) => fs.tail.foldLeft(Mul(f0, fs.head)) { case (a, b) => Mul(a, b) } }
    def branch: P[Branch] = P("if" ~/ "(" ~ boolExpr ~ ")" ~ numExpr ~ "else" ~ numExpr).map { case (c, i, e) => Branch(c, i, e) }
    P(mul | num | branch)
  }

  def expr[_ : P]: P[Expr] = P((numExpr | boolExpr) ~ End)

  def parse(text: String): Expr = {
    fastparse.parse(text, expr(_)) match {
      case Parsed.Success(result, _) => result
      case Parsed.Failure(_, _, info) => sys.error("Parsing failure! " + info)
    }
  }
}
