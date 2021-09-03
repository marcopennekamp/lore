package lore.compiler.parsing

import fastparse._
import lore.compiler.core.Fragment
import lore.compiler.syntax._

class ExpressionParserSpec extends ParserSpec[TopLevelExprNode] {
  override def parser[_: P](implicit fragment: Fragment): P[TopLevelExprNode] = {
    val nameParser = new NameParser()
    new ExpressionParser(nameParser)(fragment, ScalaWhitespace.whitespace).topLevelExpression
  }

  "The expression parser" should "reject incorrect literals" in {
    ".5".fails
    "1.".fails
    "-.5".fails
    "-1.".fails
  }

  it should "reject top-level expressions in positions not permitting them" in {
    "let x = a + return 0".fails
    "if return x then a else b".fails
    "(return 0) || b || c".fails
    "while (return x) yield do return b end".fails
    "b + return 0".fails
    "[return 2]".fails
    "#[2 -> return 0]".fails
    "%{ a: return 2 }".fails

    "if (let a = false) then do end".fails
    "while (let a = false) yield do end".fails
    "-(let a = 2)".fails
    "[let a = 2]".fails
    "#[2 -> a = 0]".fails
    "%{ a: a = 1 }".fails
  }

  it should "parse a block-rich expression within 50 milliseconds" in {
    timed(50) { () =>
      "do a + do b end + (do b end).x + b end".succeeds
    }
  }

  it should "parse a block-rich expression repeated 10,000 times within 5 seconds" in {
    timed(5000) { () =>
      val repetitions = 10000
      val source = "do " + "a + do if a < 10 then a + 10 else b + 10 end + b\n".repeat(repetitions) + " end"
      source.succeeds
    }
  }
}
