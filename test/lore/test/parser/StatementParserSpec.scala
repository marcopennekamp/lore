package lore.test.parser

import lore.test.BaseSpec
import fastparse._
import lore.ast._
import lore.parser.StatementParser

class StatementParserSpec extends BaseSpec with ParserSpecExtensions[StmtNode] {
  import StmtNode._
  import TopLevelExprNode._
  import ExprNode._

  override def parser[_: P] = StatementParser.statement

  "Strings, escapes, and interpolations" should "be parsed correctly and economically" in {
    "''" --> StringLiteralNode("")
    "'   '" --> StringLiteralNode("   ")
    "'\\n'" --> StringLiteralNode("\n")
    "'\\n\\t\\r\\'\\$\\\\'" --> StringLiteralNode("\n\t\r'$\\")
    "'test $x\\n\\t\\u0394'" --> ConcatenationNode(List(
      StringLiteralNode("test "),
      VariableNode("x"),
      StringLiteralNode("\n\t\u0394")
    ))
    "'$myLongVariable'" --> VariableNode("myLongVariable")
    "'${x}'" --> VariableNode("x")
    "'\\${quite}$some\\$confusion in ${this} town'" --> ConcatenationNode(List(
      StringLiteralNode("${quite}"),
      VariableNode("some"),
      StringLiteralNode("$confusion in "),
      VariableNode("this"),
      StringLiteralNode(" town"),
    ))
    val apples = "'${p.name}, you have $k apples. Please claim your ${if (k < 10) 'free' else '1000\\$'} apple at the reception.'"
    apples --> ConcatenationNode(List(
      PropertyAccessNode(VariableNode("p"), List("name")),
      StringLiteralNode(", you have "),
      VariableNode("k"),
      StringLiteralNode(" apples. Please claim your "),
      IfElseNode(
        LessThanNode(VariableNode("k"), IntLiteralNode(10)),
        StringLiteralNode("free"),
        StringLiteralNode("1000$"),
      ),
      StringLiteralNode(" apple at the reception."),
    ))
  }

  "Statements and top-level expressions" should "only appear in blocks, conditionals, and repetitions" in {

  }

  "Operators" should "be parsed correctly" in {
    val a = VariableNode("a")
    val b = VariableNode("b")
    val c = VariableNode("c")
    "a + b" --> AdditionNode(a, b)
    "a + { b }" --> AdditionNode(a, BlockNode(List(b)))
    "a < b" --> LessThanNode(a, b)
    "a + -b" --> AdditionNode(a, NegationNode(b))
    "~(a == b) | (a < ~b) & (~a < c)" --> DisjunctionNode(List(
      LogicalNotNode(EqualsNode(a, b)),
      ConjunctionNode(List(
        LessThanNode(a, LogicalNotNode(b)),
        LessThanNode(LogicalNotNode(a), c),
      )),
    ))
    "~a & ~b & c + -a * -b" --> ConjunctionNode(List(
      LogicalNotNode(a),
      LogicalNotNode(b),
      AdditionNode(
        c,
        MultiplicationNode(
          NegationNode(a),
          NegationNode(b),
        ),
      ),
    ))
  }

  "Comparisons" should "be parsed correctly" in {

  }

  "Conditionals and repetitions" should "be parsed correctly" in {
    "if (true) false" --> IfElseNode(BoolLiteralNode(true), BoolLiteralNode(false), UnitNode)
    "if (i < 25) { i = i + 1 }" --> IfElseNode(
      LessThanNode(VariableNode("i"), IntLiteralNode(25)),
      BlockNode(List(
        AssignmentNode(
          AddressNode(List("i")),
          AdditionNode(VariableNode("i"), IntLiteralNode(1)),
        ),
      )),
      UnitNode
    )
  }

  "A block-rich expression" should "be parsed within 50 milliseconds" in {
    timed(50) { () =>
      "{ a + { b } + { b }.x + b }" --> BlockNode(List(
        AdditionNode(AdditionNode(AdditionNode(
          VariableNode("a"),
          BlockNode(List(VariableNode("b")))),
          PropertyAccessNode(
            BlockNode(List(VariableNode("b"))),
            List("x"))
        ),
          VariableNode("b")),
      ))
    }
  }

  "A block-rich expression repeated 10,000 times" should "should be parsed within 5 seconds" in {
    timed(5000) { () =>
      val repetitions = 10000
      "{" + "a + { if (a < 10) a + 10 else b + 10 } + b\n".repeat(repetitions) + "}" --> BlockNode(
        List.fill(repetitions)(
          AdditionNode(
            AdditionNode(
              VariableNode("a"),
              BlockNode(List(IfElseNode(
                LessThanNode(VariableNode("a"), IntLiteralNode(10)),
                AdditionNode(VariableNode("a"), IntLiteralNode(10)),
                AdditionNode(VariableNode("b"), IntLiteralNode(10)),
              )))
            ),
            VariableNode("b"),
          ),
        ),
      )
    }
  }
}
