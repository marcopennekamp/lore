package lore

import lore.ast._
import lore.parser.{FileParser, FpExpressionParser}

import scala.io.Source

object Lore {
  def testCalculation(): Unit = {
    val source = Source.fromFile("examples/calculation.lore").getLines.mkString
    val parser = new FpExpressionParser()
    val expression = parser.parseExpression(source)
    val result = ExprAlgebra.evaluate(ExprAlgebra.evalAlgebra)(expression)
    println("FP  result: " + result)
  }

  def main(args: Array[String]): Unit = {
    // A new line is added at the end so the last statement has a closing newline.
    val source = Source.fromFile("examples/concat.lore").getLines.filter(_.trim.nonEmpty).mkString("\n") + "\n"
    println(source)
    val parser = new FileParser()
    val elements = parser.parse(source)

    println()
    println("Elements: ")
    elements.foreach(println)
  }
}
