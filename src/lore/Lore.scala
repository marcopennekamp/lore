package lore

import lore.algebra._
import lore.parser.{FpExpressionParser, SpcExpressionParser}
import matryoshka.implicits._
import matryoshka.data.Mu

import scala.io.Source

object Lore {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("calculation.lore").getLines.mkString

    // Parse with scala-parser-combinators
    {
      val parser = new SpcExpressionParser[Mu[Expr]]()
      val expression = parser.parseExpression(source)
      val result = expression.cata(Expr.eval) // ⇒ 24
      println("SPC result: " + result)
    }

    // Parse with FastParse
    {
      val parser = new FpExpressionParser[Mu[Expr]]()
      val expression = parser.parseExpression(source)
      val result = expression.cata(Expr.eval)
      println("FP  result: " + result)
    }
  }
}
