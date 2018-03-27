package lore

import lore.ast._
import lore.parser.FpExpressionParser

import scala.io.Source

object Lore {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("examples/calculation.lore").getLines.mkString
    val parser = new FpExpressionParser()
    val expression = parser.parseExpression(source)
    val result = ExprAlgebra.evaluate(ExprAlgebra.evalAlgebra)(expression)
    println("FP  result: " + result)
  }
}
