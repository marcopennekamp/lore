package lore.test.parser

import fastparse._
import lore.ast.StmtNode.ReturnNode
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

  it should "parse tuple, list, and map constructors correctly" in {
    // Tuple constructors.
    "()" --> UnitNode
    "(a, b)" --> TupleNode(List(va, vb))
    "(a + b, a * c, x < 5.3)" --> TupleNode(List(
      AdditionNode(va, vb), MultiplicationNode(va, vc), LessThanNode(vx, RealLiteralNode(5.3)),
    ))
    "('Hello', 'World')" --> TupleNode(List(StringLiteralNode("Hello"), StringLiteralNode("World")))

    // List constructors.
    "[]" --> ListNode(List.empty)
    "[a, b]" --> ListNode(List(va, vb))
    "[(a, b), (c, c)]" --> ListNode(List(TupleNode(List(va, vb)), TupleNode(List(vc, vc))))
    "[[a, b], ['test', 'me', 'well $c'], ['container']]" --> ListNode(List(
      ListNode(List(va, vb)),
      ListNode(List(
        StringLiteralNode("test"), StringLiteralNode("me"), ConcatenationNode(List(StringLiteralNode("well "), vc)),
      )),
      ListNode(List(StringLiteralNode("container"))),
    ))
    "[a + b, a * c, x < 5.3]" --> ListNode(List(
      AdditionNode(va, vb), MultiplicationNode(va, vc), LessThanNode(vx, RealLiteralNode(5.3)),
    ))

    // Map constructors.
    "%{ }" --> MapNode(List.empty)
    "%{ a -> 5, b -> 10 }" --> MapNode(List(KeyValueNode(va, IntLiteralNode(5)), KeyValueNode(vb, IntLiteralNode(10))))
    "%{ 'foo' -> (a, b), 'bar' -> (c, c) }" --> MapNode(List(
      KeyValueNode(StringLiteralNode("foo"), TupleNode(List(va, vb))),
      KeyValueNode(StringLiteralNode("bar"), TupleNode(List(vc, vc))),
    ))
    "%{ a -> %{ 'test' -> 'me' }, b -> %{ 'test' -> 'well $c' } }" --> MapNode(List(
      KeyValueNode(va, MapNode(List(KeyValueNode(StringLiteralNode("test"), StringLiteralNode("me"))))),
      KeyValueNode(vb, MapNode(List(KeyValueNode(StringLiteralNode("test"), ConcatenationNode(List(StringLiteralNode("well "), vc)))))),
    ))
    "%{ 1 -> a + b, 5 -> a * c, 10 -> x < 5.3 }" --> MapNode(List(
      KeyValueNode(IntLiteralNode(1), AdditionNode(va, vb)),
      KeyValueNode(IntLiteralNode(5), MultiplicationNode(va, vc)),
      KeyValueNode(IntLiteralNode(10), LessThanNode(vx, RealLiteralNode(5.3))),
    ))
  }

  it should "parse conditionals and repetitions correctly" in {
    "if (true) false" --> IfElseNode(BoolLiteralNode(true), BoolLiteralNode(false), UnitNode)
    "if (i < 25) { i += 1 }" --> IfElseNode(
      LessThanNode(vi, IntLiteralNode(25)),
      BlockNode(List(
        AssignmentNode(
          AddressNode(List("i")),
          AdditionNode(vi, IntLiteralNode(1)),
        ),
      )),
      UnitNode
    )
    "if (i <= 25) { i += 1 } else { i -= 1 }" --> IfElseNode(
      LessThanEqualsNode(vi, IntLiteralNode(25)),
      BlockNode(List(
        AssignmentNode(
          AddressNode(List("i")),
          AdditionNode(vi, IntLiteralNode(1)),
        ),
      )),
      BlockNode(List(
        AssignmentNode(
          AddressNode(List("i")),
          SubtractionNode(vi, IntLiteralNode(1)),
        ),
      )),
    )
    "if (b) return a else return c" --> IfElseNode(vb, ReturnNode(va), ReturnNode(vc))
    // Dangling else! What will the parser choose?
    "if (x) if (b) a else c" --> IfElseNode(vx, IfElseNode(vb, va, vc), UnitNode)

    // Repeat-while repetitions.
    "repeat while (a > b) a /= 2" --> RepeatWhileNode(
      GreaterThanNode(va, vb),
      AssignmentNode(AddressNode(List("a")), DivisionNode(va, IntLiteralNode(2))),
      deferCheck = false,
    )
    "repeat { println('Morning, World') } while (sunRisesOn(earth))" --> RepeatWhileNode(
      CallNode("sunRisesOn", None, List(VariableNode("earth"))),
      BlockNode(List(
        CallNode("println", None, List(StringLiteralNode("Morning, World"))),
      )),
      deferCheck = true,
    )

    // Iterations.
    """
    |{
    |  const people = ['abra', 'betty', 'carl']
    |  for (person in people) println('Hey, $person!')
    |}
    |""".stripMargin --> BlockNode(List(
      VariableDeclarationNode("people", ListNode(List(
        StringLiteralNode("abra"), StringLiteralNode("betty"), StringLiteralNode("carl"),
      )), isMutable = false),
      IterationNode(
        List(ExtractorNode("person", VariableNode("people"))),
        CallNode("println", None, List(
          ConcatenationNode(List(StringLiteralNode("Hey, "), VariableNode("person"), StringLiteralNode("!")))
        )),
      ),
    ))
    "for (a in as, b in bs) yield a + b" --> IterationNode(
      List(ExtractorNode("a", VariableNode("as")), ExtractorNode("b", VariableNode("bs"))),
      YieldNode(AdditionNode(va, vb)),
    )
  }

  it should "parse instantiation, multi-function calls, and fixed function calls correctly" in {
    "const point = Point(1, 5)" --> VariableDeclarationNode(
      "point", CallNode("Point", None, List(IntLiteralNode(1), IntLiteralNode(5))), isMutable = false,
    )
    "let position = Position3D.from2D(5.5, 6.7)" --> VariableDeclarationNode(
      "position", CallNode("Position3D", Some("from2D"), List(RealLiteralNode(5.5), RealLiteralNode(6.7))),
      isMutable = true,
    )
    "concat('stringA', 'stringB', 'stringC')" --> CallNode(
      "concat", None, List(StringLiteralNode("stringA"), StringLiteralNode("stringB"), StringLiteralNode("stringC")),
    )
    "applyDot.fixed[Dot, +Health](dot, e)" --> FixedFunctionCallNode(
      "applyDot", List(TypeExprNode.NominalNode("Dot"), TypeExprNode.ComponentNode(TypeExprNode.NominalNode("Health"))),
      List(VariableNode("dot"), VariableNode("e")),
    )

    // Calls that aren't fix can't accept type arguments.
    "applyDot.fx[Dot, +Health](dot, e)".fails
    "Point[Int](1, 5)".fails
    "Position3D.from2D[Real](5.5, 6.7)".fails
    "concat[String]('stringA', 'stringB', 'stringC')".fails
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
    // TODO: Test return statements.
    // TODO: Test yield statements.
    // TODO: Test assignments.
  }
}
