package lore

import lore.algebra._
import lore.parser.SpcExpressionParser
import matryoshka.implicits._
import matryoshka.data.Mu

import scala.io.Source

object Lore {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("calculation.lore").getLines.mkString

    // Parse with scala-parser-combinators
    val parser = new SpcExpressionParser
    val expression = parser.parseExpression[Mu[Expr]](source)
    val result = expression.cata(Expr.eval) // ⇒ 24
    println("SPC result: " + result)

    // Parse with Parboiled2
    // ...
  }
}
