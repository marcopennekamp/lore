package lore.compiler.phases.parsing.test

import fastparse._
import lore.compiler.syntax.StmtNode.ReturnNode
import lore.compiler.syntax._
import lore.compiler.core.Fragment
import lore.compiler.phases.parsing.{StatementParser, TypeParser}
import lore.compiler.test.BaseSpec
import org.scalactic.Equality

class StatementParserSpec extends BaseSpec with ParserSpecExtensions[StmtNode] {
  import ExprNode._
  import TopLevelExprNode._

  implicit private val fragment: Fragment = Fragment("Test", "")
  override def parser[_: P]: P[StmtNode] = new StatementParser(new TypeParser()).statement

  import TestNodes._

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
    "'\\${quite}$some\\$confusion in ${that} town'" --> ConcatenationNode(List(
      StringLiteralNode("${quite}"),
      VariableNode("some"),
      StringLiteralNode("$confusion in "),
      VariableNode("that"),
      StringLiteralNode(" town"),
    ))
    val apples = "'${p.name}, you have $k apples. Please claim your ${if (k < 10) 'free' else '1000\\$'} apple at the reception.'"
    apples --> ConcatenationNode(List(
      PropertyAccessNode(VariableNode("p"), "name"),
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
    "()" --> UnitNode()
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

  it should "parse conditionals and loops correctly" in {
    "if (true) false" --> IfElseNode(BoolLiteralNode(true), BoolLiteralNode(false), UnitNode())
    "if (i < 25) { i += 1 }" --> IfElseNode(
      LessThanNode(vi, IntLiteralNode(25)),
      BlockNode(List(
        AssignmentNode(
          vi,
          AdditionNode(vi, IntLiteralNode(1)),
        ),
      )),
      UnitNode()
    )
    "if (i <= 25) { i += 1 } else { i -= 1 }" --> IfElseNode(
      LessThanEqualsNode(vi, IntLiteralNode(25)),
      BlockNode(List(
        AssignmentNode(
          vi,
          AdditionNode(vi, IntLiteralNode(1)),
        ),
      )),
      BlockNode(List(
        AssignmentNode(
          vi,
          SubtractionNode(vi, IntLiteralNode(1)),
        ),
      )),
    )
    "if (b) return a else return c" --> IfElseNode(vb, StmtNode.ReturnNode(va), StmtNode.ReturnNode(vc))
    // Dangling else! What will the parser choose?
    "if (x) if (b) a else c" --> IfElseNode(vx, IfElseNode(vb, va, vc), UnitNode())

    // While loops.
    "while (a > b) a /= 2" --> RepetitionNode(
      GreaterThanNode(va, vb),
      AssignmentNode(va, DivisionNode(va, IntLiteralNode(2))),
    )
    "while (sunRisesOn(earth)) { println('Morning, World') }" --> RepetitionNode(
      SimpleCallNode("sunRisesOn", None, List(VariableNode("earth"))),
      BlockNode(List(
        SimpleCallNode("println", None, List(StringLiteralNode("Morning, World"))),
      )),
    )

    // Iterations.
    """
    |{
    |  const people: [String] = ['abra', 'betty', 'carl']
    |  for (name <- people) println('Hey, $name!')
    |}
    |""".stripMargin --> BlockNode(List(
      VariableDeclarationNode(
        "people", isMutable = false,
        Some(TypeExprNode.ListNode(tString)),
        ListNode(List(
          StringLiteralNode("abra"), StringLiteralNode("betty"), StringLiteralNode("carl"),
        )),
      ),
      IterationNode(
        List(ExtractorNode("name", VariableNode("people"))),
        SimpleCallNode("println", None, List(
          ConcatenationNode(List(StringLiteralNode("Hey, "), VariableNode("name"), StringLiteralNode("!")))
        )),
      ),
    ))
    "for (a <- as, b <- bs) a + b" --> IterationNode(
      List(ExtractorNode("a", VariableNode("as")), ExtractorNode("b", VariableNode("bs"))),
      AdditionNode(va, vb),
    )
  }

  it should "parse instantiation, multi-function calls, fixed function calls, and dynamic calls correctly" in {
    "const point = Point(1, 5)" --> VariableDeclarationNode(
      "point", isMutable = false, None,
      SimpleCallNode("Point", None, List(IntLiteralNode(1), IntLiteralNode(5))),
    )
    "let position: Position3D = Position3D.from2D(5.5, 6.7)" --> VariableDeclarationNode(
      "position", isMutable = true, Some(TypeExprNode.NominalNode("Position3D")),
      SimpleCallNode("Position3D", Some("from2D"), List(RealLiteralNode(5.5), RealLiteralNode(6.7))),
    )
    "concat('stringA', 'stringB', 'stringC')" --> SimpleCallNode(
      "concat", None, List(StringLiteralNode("stringA"), StringLiteralNode("stringB"), StringLiteralNode("stringC")),
    )
    "applyDot.fixed[Dot, +Health](dot, e)" --> FixedFunctionCallNode(
      "applyDot", List(TypeExprNode.NominalNode("Dot"), TypeExprNode.ComponentNode("Health")),
      List(VariableNode("dot"), VariableNode("e")),
    )
    "dynamic[String]('readFile', 'file.ext')" --> DynamicCallNode(
      TypeExprNode.NominalNode("String"),
      List(
        StringLiteralNode("readFile"),
        StringLiteralNode("file.ext"),
      ),
    )

    // Calls that aren't fix can't accept type arguments.
    "applyDot.fx[Dot, +Health](dot, e)".fails
    "Point[Int](1, 5)".fails
    "Position3D.from2D[Real](5.5, 6.7)".fails
    "concat[String]('stringA', 'stringB', 'stringC')".fails
  }

  it should "parse variable declarations and assignments correctly" in {
    "a.b.c = x" --> AssignmentNode(
      PropertyAccessNode(PropertyAccessNode(va, "b"), "c"),
      vx,
    )
    "const x: Int = a" --> VariableDeclarationNode("x", isMutable = false, Some(TypeExprNode.NominalNode("Int")), va)
    "let y: Real = b" --> VariableDeclarationNode("y", isMutable = true, Some(TypeExprNode.NominalNode("Real")), vb)
    "const z: E & +C1 & +C2 & L = c" --> VariableDeclarationNode(
      "z", isMutable = false,
      Some(TypeExprNode.IntersectionNode(List(
        TypeExprNode.NominalNode("E"), TypeExprNode.ComponentNode("C1"),
        TypeExprNode.ComponentNode("C2"), TypeExprNode.NominalNode("L"),
      ))),
      vc,
    )
  }

  it should "assign the correct indices" in {
    inside("(a + b, a * c, x < 5.3)".parsed) {
      case tuple: TupleNode =>
        tuple.position.index shouldEqual 0
        inside(tuple.expressions) {
          case Seq(add: AdditionNode, mult: MultiplicationNode, comp: LessThanNode) =>
            add.position.index shouldEqual 1
            add.left.position.index shouldEqual 1
            add.right.position.index shouldEqual 5
            mult.position.index shouldEqual 8
            mult.left.position.index shouldEqual 8
            mult.right.position.index shouldEqual 12
            comp.position.index shouldEqual 15
            comp.left.position.index shouldEqual 15
            comp.right.position.index shouldEqual 19
        }
    }
    inside("if (i <= 25) { i += 1 } else { i -= 1 }".parsed) {
      case ifElse: IfElseNode =>
        ifElse.position.index shouldEqual 0
        inside(ifElse.condition) {
          case lt: LessThanEqualsNode =>
            lt.position.index shouldEqual 4
            lt.left.position.index shouldEqual 4
            lt.right.position.index shouldEqual 9
        }
        inside(ifElse.onTrue) {
          case block: BlockNode =>
            block.position.index shouldEqual 13
            inside(block.statements) {
              case Seq(assign: AssignmentNode) =>
                assign.position.index shouldEqual 15
                assign.address.position.index shouldEqual 15
                assign.value.position.index shouldEqual 20
            }
        }
        inside(ifElse.onFalse) {
          case block: BlockNode =>
            block.position.index shouldEqual 29
            inside(block.statements) {
              case Seq(assign: AssignmentNode) =>
                assign.position.index shouldEqual 31
                assign.address.position.index shouldEqual 31
                assign.value.position.index shouldEqual 36
            }
        }
    }

    inside("let position: Position3D = Position3D.from2D(5.5, 6.7)".parsed) {
      case decl: VariableDeclarationNode =>
        decl.position.index shouldEqual 0
        decl.tpe.value.position.index shouldEqual 14
        inside(decl.value) {
          case call: SimpleCallNode =>
            call.position.index shouldEqual 27
            inside(call.arguments) {
              case Seq(rl1: RealLiteralNode, rl2: RealLiteralNode) =>
                rl1.position.index shouldEqual 45
                rl2.position.index shouldEqual 50
            }
        }
    }

    inside("applyDot.fixed[Dot, +Health](dot, e)".parsed) {
      case call: FixedFunctionCallNode =>
        call.position.index shouldEqual 0
        inside(call.types) {
          case Seq(t1: TypeExprNode.NominalNode, t2: TypeExprNode.ComponentNode) =>
            t1.position.index shouldEqual 15
            t2.position.index shouldEqual 20
        }
        inside(call.arguments) {
          case Seq(dot: VariableNode, e: VariableNode) =>
            dot.position.index shouldEqual 29
            e.position.index shouldEqual 34
        }
    }

    inside("%{ a -> %{ 'test' -> 'me' }, b -> %{ 'test' -> 'well $c' } }".parsed) {
      case map: MapNode =>
        map.position.index shouldEqual 0
        inside(map.kvs) {
          case Seq(a: KeyValueNode, b: KeyValueNode) =>
            a.position.index shouldEqual 3
            a.key.position.index shouldEqual 3
            inside(a.value) {
              case innerMap: MapNode =>
                innerMap.position.index shouldEqual 8
                inside(innerMap.kvs) {
                  case Seq(test: KeyValueNode) =>
                    test.position.index shouldEqual 11
                    test.key.position.index shouldEqual 11
                    test.value.position.index shouldEqual 21
                }
            }
            b.position.index shouldEqual 29
            b.key.position.index shouldEqual 29
            inside(b.value) {
              case innerMap: MapNode =>
                innerMap.position.index shouldEqual 34
                inside(innerMap.kvs) {
                  case Seq(test: KeyValueNode) =>
                    test.position.index shouldEqual 37
                    test.key.position.index shouldEqual 37
                    test.value.position.index shouldEqual 47
                }
            }
        }
    }
  }

  it should "parse a block-rich expression within 50 milliseconds" in {
    timed(50) { () =>
      "{ a + { b } + { b }.x + b }" --> BlockNode(List(
        AdditionNode(
          AdditionNode(
            AdditionNode(va, BlockNode(List(vb))),
            PropertyAccessNode(BlockNode(List(vb)), "x"),
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
    "const x = a + return 0".fails
    "if (return x) a else b".fails
    "(yield 0) | b | c".fails
    "repeat while (yield x) { yield b }".fails
    "b + yield 0".fails
    "if (const a = false) { }".fails
    "repeat while (const a = false) { }".fails
    "-(const a = 2)".fails

    "b + { const a = 4 \n a * a }" --> AdditionNode(
      vb,
      BlockNode(List(
        VariableDeclarationNode("a", isMutable = false, None, IntLiteralNode(4)),
        MultiplicationNode(va, va),
      )),
    )
    "if (false) const a = 0" --> IfElseNode(
      BoolLiteralNode(false),
      VariableDeclarationNode("a", isMutable = false, None, IntLiteralNode(0)),
      UnitNode(),
    )
    "for (e <- list) const a = e" --> IterationNode(
      List(ExtractorNode("e", VariableNode("list"))),
      VariableDeclarationNode("a", isMutable = false, None, VariableNode("e")),
    )
    "if (false) return 0 else 0" --> IfElseNode(
      BoolLiteralNode(false),
      ReturnNode(IntLiteralNode(0)),
      IntLiteralNode(0),
    )
    "for (e <- list) return e" --> IterationNode(
      List(ExtractorNode("e", VariableNode("list"))),
      ReturnNode(VariableNode("e")),
    )
    "for (e <- list) e" --> IterationNode(
      List(ExtractorNode("e", VariableNode("list"))),
      VariableNode("e"),
    )
  }
}
