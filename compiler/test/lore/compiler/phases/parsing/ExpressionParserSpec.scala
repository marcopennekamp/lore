package lore.compiler.phases.parsing

import fastparse._
import lore.compiler.core.Fragment
import lore.compiler.syntax._

class ExpressionParserSpec extends ParserSpec[TopLevelExprNode] {
  override def parser[_: P](implicit fragment: Fragment): P[TopLevelExprNode] = new ExpressionParser(new TypeParser()).topLevelExpression

  "The expression parser" should "reject incorrect literals" in {
    ".5".fails
    "1.".fails
    "-.5".fails
    "-1.".fails
  }

  it should "reject top-level expressions in positions not permitting them" in {
    "let x = a + return 0".fails
    "if (return x) a else b".fails
    "(return 0) || b || c".fails
    "while (return x) { return b }".fails
    "b + return 0".fails
    "[return 2]".fails
    "#[2 -> return 0]".fails
    "%{ a: return 2 }".fails

    "if (let a = false) { }".fails
    "while (let a = false) { }".fails
    "-(let a = 2)".fails
    "[let a = 2]".fails
    "#[2 -> a = 0]".fails
    "%{ a: a = 1 }".fails
  }

  it should "parse a block-rich expression within 50 milliseconds" in {
    timed(50) { () =>
      "{ a + { b } + { b }.x + b }".succeeds
    }
  }

  it should "parse a block-rich expression repeated 10,000 times within 5 seconds" in {
    timed(5000) { () =>
      val repetitions = 10000
      val source = "{" + "a + { if (a < 10) a + 10 else b + 10 } + b\n".repeat(repetitions) + "}"
      source.succeeds
    }
  }
}
