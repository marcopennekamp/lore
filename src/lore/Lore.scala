package lore

import lore.ast._
import lore.exceptions.FunctionNotFoundException
import lore.execution.Context
import lore.parser.{FragmentParser, FpExpressionParser}

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
    val source = Source.fromFile(s"examples/${args(0)}.lore").getLines.filter(_.trim.nonEmpty).mkString("\n") + "\n"
    //println(source)
    val parser = new FragmentParser()
    val elements = parser.parse(source)
    implicit val context = Context.build(elements)

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
    context.verify()

    println()
    println("Function fit for each call statement:")
    context.calls.foreach { call =>
      val multiFunction = context.multiFunctions.getOrElse(call.functionName, throw FunctionNotFoundException(call.functionName))
      val fit = multiFunction.fit(call.argumentType)
      val min = fit.multiMin
      println(s"Fit: $fit")
      println(s"Min: $min")
      println()
    }
  }
}
