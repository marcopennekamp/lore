package lore.compiler.phases.parsing.test

import fastparse._
import lore.compiler.syntax._
import lore.compiler.core.Fragment
import lore.compiler.phases.parsing.{StatementParser, TypeParser}
import lore.compiler.test.BaseSpec
import org.scalactic.Equality

class StatementParserSpec extends BaseSpec with ParserSpecExtensions[StmtNode] {
  implicit private val fragment: Fragment = Fragment("Test", "")
  override def parser[_: P]: P[StmtNode] = new StatementParser(new TypeParser()).statement

  import TestNodes._

  "The statement parser" should "parse strings, escapes, and interpolations correctly" in {
    "''" --> Stmt.StringLiteral("")
    "'   '" --> Stmt.StringLiteral("   ")
    "'\\n'" --> Stmt.StringLiteral("\n")
    "'\\n\\t\\r\\'\\$\\\\'" --> Stmt.StringLiteral("\n\t\r'$\\")
    "'test $x\\n\\t\\u0394'" --> Stmt.Concatenation(List(
      Stmt.StringLiteral("test "),
      vx,
      Stmt.StringLiteral("\n\t\u0394"),
    ))
    "'$myLongVariable'" --> Stmt.Variable("myLongVariable")
    "'${x}'" --> vx
    "'\\${quite}$some\\$confusion in ${that} town'" --> Stmt.Concatenation(List(
      Stmt.StringLiteral("${quite}"),
      Stmt.Variable("some"),
      Stmt.StringLiteral("$confusion in "),
      Stmt.Variable("that"),
      Stmt.StringLiteral(" town"),
    ))
    val apples = "'${p.name}, you have $k apples. Please claim your ${if (k < 10) 'free' else '1000\\$'} apple at the reception.'"
    apples --> Stmt.Concatenation(List(
      Stmt.PropertyAccess(Stmt.Variable("p"), "name"),
      Stmt.StringLiteral(", you have "),
      vk,
      Stmt.StringLiteral(" apples. Please claim your "),
      Stmt.IfElse(
        Stmt.LessThan(vk, Stmt.IntLiteral(10)),
        Stmt.StringLiteral("free"),
        Stmt.StringLiteral("1000$"),
      ),
      Stmt.StringLiteral(" apple at the reception."),
    ))
  }

  it should "parse literals correctly" in {
    "0" --> Stmt.IntLiteral(0)
    "-15" --> Stmt.IntLiteral(-15)
    "0.0" --> Stmt.RealLiteral(0)
    "1.5" --> Stmt.RealLiteral(1.5)
    "-1.5" --> Stmt.RealLiteral(-1.5)
    ".5".fails
    "1.".fails
    "true" --> Stmt.BoolLiteral(true)
    "false" --> Stmt.BoolLiteral(false)
  }

  it should "parse operators correctly" in {
    "a + b" --> Stmt.Addition(va, vb)
    "a - b" --> Stmt.Subtraction(va, vb)
    "a * b + b * a" --> Stmt.Addition(Stmt.Multiplication(va, vb), Stmt.Multiplication(vb, va))
    "a / b" --> Stmt.Division(va, vb)
    "a < b" --> Stmt.LessThan(va, vb)
    "a <= b" --> Stmt.LessThanEquals(va, vb)
    "a > b" --> Stmt.GreaterThan(va, vb)
    "a >= b" --> Stmt.GreaterThanEquals(va, vb)
    "a == b" --> Stmt.Equals(va, vb)
    "a =/= b" --> Stmt.NotEquals(va, vb)
    "a | b | c" --> Stmt.Disjunction(List(va, vb, vc))
    "a & b & c" --> Stmt.Conjunction(List(va, vb, vc))
  }

  it should "parse complex operator expressions correctly" in {
    "a + { b }" --> Stmt.Addition(va, Stmt.Block(List(vb)))
    "a + -b" --> Stmt.Addition(va, Stmt.Negation(vb))
    "~(a == b) | (a < ~b) & (~a < c)" --> Stmt.Disjunction(List(
      Stmt.LogicalNot(Stmt.Equals(va, vb)),
      Stmt.Conjunction(List(
        Stmt.LessThan(va, Stmt.LogicalNot(vb)),
        Stmt.LessThan(Stmt.LogicalNot(va), vc),
      )),
    ))
    "~a & ~b & c + -a * -b" --> Stmt.Conjunction(List(
      Stmt.LogicalNot(va),
      Stmt.LogicalNot(vb),
      Stmt.Addition(
        vc,
        Stmt.Multiplication(
          Stmt.Negation(va),
          Stmt.Negation(vb),
        ),
      ),
    ))
  }

  it should "parse tuple, list, and map constructors correctly" in {
    // Tuple constructors.
    "()" --> Stmt.Unit()
    "(a, b)" --> Stmt.Tuple(List(va, vb))
    "(a + b, a * c, x < 5.3)" --> Stmt.Tuple(List(
      Stmt.Addition(va, vb), Stmt.Multiplication(va, vc), Stmt.LessThan(vx, Stmt.RealLiteral(5.3)),
    ))
    "('Hello', 'World')" --> Stmt.Tuple(List(Stmt.StringLiteral("Hello"), Stmt.StringLiteral("World")))

    // List constructors.
    "[]" --> Stmt.List(List.empty)
    "[a, b]" --> Stmt.List(List(va, vb))
    "[(a, b), (c, c)]" --> Stmt.List(List(Stmt.Tuple(List(va, vb)), Stmt.Tuple(List(vc, vc))))
    "[[a, b], ['test', 'me', 'well $c'], ['container']]" --> Stmt.List(List(
      Stmt.List(List(va, vb)),
      Stmt.List(List(
        Stmt.StringLiteral("test"), Stmt.StringLiteral("me"), Stmt.Concatenation(List(Stmt.StringLiteral("well "), vc)),
      )),
      Stmt.List(List(Stmt.StringLiteral("container"))),
    ))
    "[a + b, a * c, x < 5.3]" --> Stmt.List(List(
      Stmt.Addition(va, vb), Stmt.Multiplication(va, vc), Stmt.LessThan(vx, Stmt.RealLiteral(5.3)),
    ))

    // Map constructors.
    "%{ }" --> Stmt.Map(List.empty)
    "%{ a -> 5, b -> 10 }" --> Stmt.Map(List(Stmt.KeyValue(va, Stmt.IntLiteral(5)), Stmt.KeyValue(vb, Stmt.IntLiteral(10))))
    "%{ 'foo' -> (a, b), 'bar' -> (c, c) }" --> Stmt.Map(List(
      Stmt.KeyValue(Stmt.StringLiteral("foo"), Stmt.Tuple(List(va, vb))),
      Stmt.KeyValue(Stmt.StringLiteral("bar"), Stmt.Tuple(List(vc, vc))),
    ))
    "%{ a -> %{ 'test' -> 'me' }, b -> %{ 'test' -> 'well $c' } }" --> Stmt.Map(List(
      Stmt.KeyValue(va, Stmt.Map(List(Stmt.KeyValue(Stmt.StringLiteral("test"), Stmt.StringLiteral("me"))))),
      Stmt.KeyValue(vb, Stmt.Map(List(Stmt.KeyValue(Stmt.StringLiteral("test"), Stmt.Concatenation(List(Stmt.StringLiteral("well "), vc)))))),
    ))
    "%{ 1 -> a + b, 5 -> a * c, 10 -> x < 5.3 }" --> Stmt.Map(List(
      Stmt.KeyValue(Stmt.IntLiteral(1), Stmt.Addition(va, vb)),
      Stmt.KeyValue(Stmt.IntLiteral(5), Stmt.Multiplication(va, vc)),
      Stmt.KeyValue(Stmt.IntLiteral(10), Stmt.LessThan(vx, Stmt.RealLiteral(5.3))),
    ))
  }

  it should "parse conditionals and loops correctly" in {
    "if (true) false" --> Stmt.IfElse(Stmt.BoolLiteral(true), Stmt.BoolLiteral(false), Stmt.Unit())
    "if (i < 25) { i += 1 }" --> Stmt.IfElse(
      Stmt.LessThan(vi, Stmt.IntLiteral(25)),
      Stmt.Block(List(
        Stmt.Assignment(
          vi,
          Stmt.Addition(vi, Stmt.IntLiteral(1)),
        ),
      )),
      Stmt.Unit()
    )
    "if (i <= 25) { i += 1 } else { i -= 1 }" --> Stmt.IfElse(
      Stmt.LessThanEquals(vi, Stmt.IntLiteral(25)),
      Stmt.Block(List(
        Stmt.Assignment(
          vi,
          Stmt.Addition(vi, Stmt.IntLiteral(1)),
        ),
      )),
      Stmt.Block(List(
        Stmt.Assignment(
          vi,
          Stmt.Subtraction(vi, Stmt.IntLiteral(1)),
        ),
      )),
    )
    "if (b) return a else return c" --> Stmt.IfElse(vb, Stmt.Return(va), Stmt.Return(vc))
    // Dangling else! What will the parser choose?
    "if (x) if (b) a else c" --> Stmt.IfElse(vx, Stmt.IfElse(vb, va, vc), Stmt.Unit())

    // While loops.
    "while (a > b) a /= 2" --> Stmt.Repetition(
      Stmt.GreaterThan(va, vb),
      Stmt.Assignment(va, Stmt.Division(va, Stmt.IntLiteral(2))),
    )
    "while (sunRisesOn(earth)) { println('Morning, World') }" --> Stmt.Repetition(
      Stmt.SimpleCall("sunRisesOn", None, List(Stmt.Variable("earth"))),
      Stmt.Block(List(
        Stmt.SimpleCall("println", None, List(Stmt.StringLiteral("Morning, World"))),
      )),
    )

    // Iterations.
    """
    |{
    |  const people: [String] = ['abra', 'betty', 'carl']
    |  for (name <- people) println('Hey, $name!')
    |}
    |""".stripMargin --> Stmt.Block(List(
      Stmt.VariableDeclaration(
        "people", false,
        Some(Type.List(tString)),
        Stmt.List(List(
          Stmt.StringLiteral("abra"), Stmt.StringLiteral("betty"), Stmt.StringLiteral("carl"),
        )),
      ),
      Stmt.Iteration(
        List(Stmt.Extractor("name", Stmt.Variable("people"))),
        Stmt.SimpleCall("println", None, List(
          Stmt.Concatenation(List(Stmt.StringLiteral("Hey, "), Stmt.Variable("name"), Stmt.StringLiteral("!")))
        )),
      ),
    ))
    "for (a <- as, b <- bs) a + b" --> Stmt.Iteration(
      List(Stmt.Extractor("a", Stmt.Variable("as")), Stmt.Extractor("b", Stmt.Variable("bs"))),
      Stmt.Addition(va, vb),
    )
  }

  it should "parse instantiation, multi-function calls, fixed function calls, and dynamic calls correctly" in {
    "const point = Point(1, 5)" --> Stmt.VariableDeclaration(
      "point", false, None,
      Stmt.SimpleCall("Point", None, List(Stmt.IntLiteral(1), Stmt.IntLiteral(5))),
    )
    "let position: Position3D = Position3D.from2D(5.5, 6.7)" --> Stmt.VariableDeclaration(
      "position", true, Some(Type.Nominal("Position3D")),
      Stmt.SimpleCall("Position3D", Some("from2D"), List(Stmt.RealLiteral(5.5), Stmt.RealLiteral(6.7))),
    )
    "concat('stringA', 'stringB', 'stringC')" --> Stmt.SimpleCall(
      "concat", None, List(Stmt.StringLiteral("stringA"), Stmt.StringLiteral("stringB"), Stmt.StringLiteral("stringC")),
    )
    "applyDot.fixed[Dot, +Health](dot, e)" --> Stmt.FixedFunctionCall(
      "applyDot", List(Type.Nominal("Dot"), Type.Component("Health")),
      List(Stmt.Variable("dot"), Stmt.Variable("e")),
    )
    "dynamic[String]('readFile', 'file.ext')" --> Stmt.DynamicCall(
      Type.Nominal("String"),
      List(
        Stmt.StringLiteral("readFile"),
        Stmt.StringLiteral("file.ext"),
      ),
    )

    // Calls that aren't fix can't accept type arguments.
    "applyDot.fx[Dot, +Health](dot, e)".fails
    "Point[Int](1, 5)".fails
    "Position3D.from2D[Real](5.5, 6.7)".fails
    "concat[String]('stringA', 'stringB', 'stringC')".fails
  }

  it should "parse variable declarations and assignments correctly" in {
    "a.b.c = x" --> Stmt.Assignment(
      Stmt.PropertyAccess(Stmt.PropertyAccess(va, "b"), "c"),
      vx,
    )
    "const x: Int = a" --> Stmt.VariableDeclaration("x", false, Some(Type.Nominal("Int")), va)
    "let y: Real = b" --> Stmt.VariableDeclaration("y", true, Some(Type.Nominal("Real")), vb)
    "const z: E & +C1 & +C2 & L = c" --> Stmt.VariableDeclaration(
      "z", false,
      Some(Type.Intersection(List(
        Type.Nominal("E"), Type.Component("C1"),
        Type.Component("C2"), Type.Nominal("L"),
      ))),
      vc,
    )
  }

  it should "assign the correct indices" in {
    inside("(a + b, a * c, x < 5.3)".parsed) {
      case tuple: ExprNode.TupleNode =>
        tuple.position.index shouldEqual 0
        inside(tuple.expressions) {
          case Seq(add: ExprNode.AdditionNode, mult: ExprNode.MultiplicationNode, comp: ExprNode.LessThanNode) =>
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
      case ifElse: ExprNode.IfElseNode =>
        ifElse.position.index shouldEqual 0
        inside(ifElse.condition) {
          case lt: ExprNode.LessThanEqualsNode =>
            lt.position.index shouldEqual 4
            lt.left.position.index shouldEqual 4
            lt.right.position.index shouldEqual 9
        }
        inside(ifElse.onTrue) {
          case block: ExprNode.BlockNode =>
            block.position.index shouldEqual 13
            inside(block.statements) {
              case Seq(assign: TopLevelExprNode.AssignmentNode) =>
                assign.position.index shouldEqual 15
                assign.address.position.index shouldEqual 15
                assign.value.position.index shouldEqual 20
            }
        }
        inside(ifElse.onFalse) {
          case block: ExprNode.BlockNode =>
            block.position.index shouldEqual 29
            inside(block.statements) {
              case Seq(assign: TopLevelExprNode.AssignmentNode) =>
                assign.position.index shouldEqual 31
                assign.address.position.index shouldEqual 31
                assign.value.position.index shouldEqual 36
            }
        }
    }

    inside("let position: Position3D = Position3D.from2D(5.5, 6.7)".parsed) {
      case decl: TopLevelExprNode.VariableDeclarationNode =>
        decl.position.index shouldEqual 0
        decl.tpe.value.position.index shouldEqual 14
        inside(decl.value) {
          case call: ExprNode.SimpleCallNode =>
            call.position.index shouldEqual 27
            inside(call.arguments) {
              case Seq(rl1: ExprNode.RealLiteralNode, rl2: ExprNode.RealLiteralNode) =>
                rl1.position.index shouldEqual 45
                rl2.position.index shouldEqual 50
            }
        }
    }

    inside("applyDot.fixed[Dot, +Health](dot, e)".parsed) {
      case call: ExprNode.FixedFunctionCallNode =>
        call.position.index shouldEqual 0
        inside(call.types) {
          case Seq(t1: TypeExprNode.NominalNode, t2: TypeExprNode.ComponentNode) =>
            t1.position.index shouldEqual 15
            t2.position.index shouldEqual 20
        }
        inside(call.arguments) {
          case Seq(dot: ExprNode.VariableNode, e: ExprNode.VariableNode) =>
            dot.position.index shouldEqual 29
            e.position.index shouldEqual 34
        }
    }

    inside("%{ a -> %{ 'test' -> 'me' }, b -> %{ 'test' -> 'well $c' } }".parsed) {
      case map: ExprNode.MapNode =>
        map.position.index shouldEqual 0
        inside(map.kvs) {
          case Seq(a: ExprNode.KeyValueNode, b: ExprNode.KeyValueNode) =>
            a.position.index shouldEqual 3
            a.key.position.index shouldEqual 3
            inside(a.value) {
              case innerMap: ExprNode.MapNode =>
                innerMap.position.index shouldEqual 8
                inside(innerMap.kvs) {
                  case Seq(test: ExprNode.KeyValueNode) =>
                    test.position.index shouldEqual 11
                    test.key.position.index shouldEqual 11
                    test.value.position.index shouldEqual 21
                }
            }
            b.position.index shouldEqual 29
            b.key.position.index shouldEqual 29
            inside(b.value) {
              case innerMap: ExprNode.MapNode =>
                innerMap.position.index shouldEqual 34
                inside(innerMap.kvs) {
                  case Seq(test: ExprNode.KeyValueNode) =>
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
      "{ a + { b } + { b }.x + b }" --> Stmt.Block(List(
        Stmt.Addition(
          Stmt.Addition(
            Stmt.Addition(va, Stmt.Block(List(vb))),
            Stmt.PropertyAccess(Stmt.Block(List(vb)), "x"),
          ),
          vb,
        ),
      ))
    }
  }

  it should "parse a block-rich expression repeated 10,000 times within 5 seconds" in {
    timed(5000) { () =>
      val repetitions = 10000
      "{" + "a + { if (a < 10) a + 10 else b + 10 } + b\n".repeat(repetitions) + "}" --> Stmt.Block(
        List.fill(repetitions)(
          Stmt.Addition(
            Stmt.Addition(
              va,
              Stmt.Block(List(Stmt.IfElse(
                Stmt.LessThan(va, Stmt.IntLiteral(10)),
                Stmt.Addition(va, Stmt.IntLiteral(10)),
                Stmt.Addition(vb, Stmt.IntLiteral(10)),
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

    "b + { const a = 4 \n a * a }" --> Stmt.Addition(
      vb,
      Stmt.Block(List(
        Stmt.VariableDeclaration("a", false, None, Stmt.IntLiteral(4)),
        Stmt.Multiplication(va, va),
      )),
    )
    "if (false) const a = 0" --> Stmt.IfElse(
      Stmt.BoolLiteral(false),
      Stmt.VariableDeclaration("a", false, None, Stmt.IntLiteral(0)),
      Stmt.Unit(),
    )
    "for (e <- list) const a = e" --> Stmt.Iteration(
      List(Stmt.Extractor("e", Stmt.Variable("list"))),
      Stmt.VariableDeclaration("a", false, None, Stmt.Variable("e")),
    )
    "if (false) return 0 else 0" --> Stmt.IfElse(
      Stmt.BoolLiteral(false),
      Stmt.Return(Stmt.IntLiteral(0)),
      Stmt.IntLiteral(0),
    )
    "for (e <- list) return e" --> Stmt.Iteration(
      List(Stmt.Extractor("e", Stmt.Variable("list"))),
      Stmt.Return(Stmt.Variable("e")),
    )
    "for (e <- list) e" --> Stmt.Iteration(
      List(Stmt.Extractor("e", Stmt.Variable("list"))),
      Stmt.Variable("e"),
    )
  }
}
