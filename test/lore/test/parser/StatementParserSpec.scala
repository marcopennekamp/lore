package lore.test.parser

import fastparse._
import lore.ast._
import lore.parser.StatementParser
import lore.test.BaseSpec

class StatementParserSpec extends BaseSpec with ParserSpecExtensions[StmtNode] {
  import ExprNode._
  import TopLevelExprNode._

  override def parser[_: P] = StatementParser.statement

  private val va = VariableNode("a")
  private val vb = VariableNode("b")
  private val vc = VariableNode("c")
  private val vi = VariableNode("i")
  private val vk = VariableNode("k")
  private val vx = VariableNode("x")

  "The statement parser" should "parse strings, escapes, and interpolations correctly" in {
    "''" --> StringLiteralNode("")
    "'   '" --> StringLiteralNode("   ")
    "'\\n'" --> StringLiteralNode("\n")
    "'\\n\\t\\r\\'\\$\\\\'" --> StringLiteralNode("\n\t\r'$\\")
    "'test $x\\n\\t\\u0394'" --> ConcatenationNode(List(
      StringLiteralNode("test "),
      vx,
      StringLiteralNode("\n\t\u0394"),
    ))
    "'$myLongVariable'" --> VariableNode("myLongVariable")
    "'${x}'" --> vx
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
      vk,
      StringLiteralNode(" apples. Please claim your "),
      IfElseNode(
        LessThanNode(vk, IntLiteralNode(10)),
        StringLiteralNode("free"),
        StringLiteralNode("1000$"),
      ),
      StringLiteralNode(" apple at the reception."),
    ))
  }

  it should "parse literals correctly" in {
    "0" --> IntLiteralNode(0)
    "-15" --> IntLiteralNode(-15)
    "0.0" --> RealLiteralNode(0)
    "1.5" --> RealLiteralNode(1.5)
    "-1.5" --> RealLiteralNode(-1.5)
    ".5".fails
    "1.".fails
    "true" --> BoolLiteralNode(true)
    "false" --> BoolLiteralNode(false)
  }

  it should "parse operators correctly" in {
    "a + b" --> AdditionNode(va, vb)
    "a - b" --> SubtractionNode(va, vb)
    "a * b + b * a" --> AdditionNode(MultiplicationNode(va, vb), MultiplicationNode(vb, va))
    "a / b" --> DivisionNode(va, vb)
    "a < b" --> LessThanNode(va, vb)
    "a <= b" --> LessThanEqualsNode(va, vb)
    "a > b" --> GreaterThanNode(va, vb)
    "a >= b" --> GreaterThanEqualsNode(va, vb)
    "a == b" --> EqualsNode(va, vb)
    "a =/= b" --> NotEqualsNode(va, vb)
    "a | b | c" --> DisjunctionNode(List(va, vb, vc))
    "a & b & c" --> ConjunctionNode(List(va, vb, vc))
  }

  it should "parse complex operator expressions correctly" in {
    "a + { b }" --> AdditionNode(va, BlockNode(List(vb)))
    "a + -b" --> AdditionNode(va, NegationNode(vb))
    "~(a == b) | (a < ~b) & (~a < c)" --> DisjunctionNode(List(
      LogicalNotNode(EqualsNode(va, vb)),
      ConjunctionNode(List(
        LessThanNode(va, LogicalNotNode(vb)),
        LessThanNode(LogicalNotNode(va), vc),
      )),
    ))
    "~a & ~b & c + -a * -b" --> ConjunctionNode(List(
      LogicalNotNode(va),
      LogicalNotNode(vb),
      AdditionNode(
        vc,
        MultiplicationNode(
          NegationNode(va),
          NegationNode(vb),
        ),
      ),
    ))
  }

  it should "parse conditionals and repetitions correctly" in {
    "if (true) false" --> IfElseNode(BoolLiteralNode(true), BoolLiteralNode(false), UnitNode)
    "if (i < 25) { i = i + 1 }" --> IfElseNode(
      LessThanNode(vi, IntLiteralNode(25)),
      BlockNode(List(
        AssignmentNode(
          AddressNode(List("i")),
          AdditionNode(vi, IntLiteralNode(1)),
        ),
      )),
      UnitNode
    )
  }

  it should "parse a block-rich expression within 50 milliseconds" in {
    timed(50) { () =>
      "{ a + { b } + { b }.x + b }" --> BlockNode(List(
        AdditionNode(
          AdditionNode(
            AdditionNode(va, BlockNode(List(vb))),
            PropertyAccessNode(BlockNode(List(vb)), List("x")),
          ),
          vb,
        ),
      ))
    }
  }

  it should "parse a block-rich expression repeated 10,000 times within 5 seconds" in {
    timed(5000) { () =>
      val repetitions = 10000
      "{" + "a + { if (a < 10) a + 10 else b + 10 } + b\n".repeat(repetitions) + "}" --> BlockNode(
        List.fill(repetitions)(
          AdditionNode(
            AdditionNode(
              va,
              BlockNode(List(IfElseNode(
                LessThanNode(va, IntLiteralNode(10)),
                AdditionNode(va, IntLiteralNode(10)),
                AdditionNode(vb, IntLiteralNode(10)),
              )))
            ),
            vb,
          ),
        ),
      )
    }
  }

  "Statements and top-level expressions" should "only appear in blocks, conditionals, and repetitions" in {

  }
}
