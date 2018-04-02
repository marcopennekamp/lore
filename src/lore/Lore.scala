package lore

import lore.ast._
import lore.exceptions.FunctionNotFoundException
import lore.execution.Context
import lore.parser.ExpressionParser

import scala.io.Source

object Lore {
  def testCalculation(): Unit = {
    val source = Source.fromFile("examples/calculation.lore").getLines.mkString
    val expression = ExpressionParser.parse(source)
    val result = ExprAlgebra.evaluate(ExprAlgebra.evalAlgebra)(expression)
    println("FP  result: " + result)
  }

  def main(args: Array[String]): Unit = {
    implicit val context = Context.fromExample(args(0))

    // Print types for debugging.
    println("Types:")
    context.types.values.foreach { t =>
      println(s"  ${t.verbose}")
    }

    // Print functions for debugging.
    context.multiFunctions.values.foreach { mf =>
      println()
      println(s"${mf.name}:")
      mf.functions.foreach(f => println(s"  $f"))
    }

    println()
    val verificationResult = context.verify()
    verificationResult.print()
  }
}
