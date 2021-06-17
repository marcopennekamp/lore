package lore.compiler.phases.parsing

import fastparse._
import lore.compiler.core.Fragment
import lore.compiler.syntax._

// TODO: Implement these tests using functional tests.

class ExpressionParserSpec extends ParserSpec[TopLevelExprNode] {
  override def parser[_: P](implicit fragment: Fragment): P[TopLevelExprNode] = new ExpressionParser(new TypeParser()).topLevelExpression

  import TestNodes._

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

  it should "parse loops correctly" in {
    // While loops.
    "while (a > b) a /= 2" --> Stmt.Repetition(
      Stmt.GreaterThan(va, vb),
      Stmt.Assignment(va, Stmt.Division(va, Stmt.IntLiteral(2))),
    )
    /* "while (sunRisesOn(earth)) { println('Morning, World') }" --> Stmt.Repetition(
      Stmt.SimpleCall("sunRisesOn", Vector(Stmt.Variable("earth"))),
      Stmt.Block(Vector(
        Stmt.SimpleCall("println", Vector(Stmt.StringLiteral("Morning, World"))),
      )),
    ) */

    // Iterations.
    /* """
    |{
    |  let people: [String] = ['abra', 'betty', 'carl']
    |  for (name <- people) println('Hey, $name!')
    |}
    |""".stripMargin --> Stmt.Block(Vector(
      Stmt.VariableDeclaration(
        "people", false,
        Some(Type.List(tString)),
        Stmt.List(Vector(
          Stmt.StringLiteral("abra"), Stmt.StringLiteral("betty"), Stmt.StringLiteral("carl"),
        )),
      ),
      Stmt.Iteration(
        Vector(Stmt.Extractor("name", Stmt.Variable("people"))),
        Stmt.SimpleCall("println", Vector(
          Stmt.Concatenation(Vector(Stmt.StringLiteral("Hey, "), Stmt.Variable("name"), Stmt.StringLiteral("!")))
        )),
      ),
    )) */
    "for (a <- as, b <- bs) a + b" --> Stmt.Iteration(
      Vector(Stmt.Extractor("a", Stmt.Variable("as")), Stmt.Extractor("b", Stmt.Variable("bs"))),
      Stmt.Addition(va, vb),
    )
  }

  it should "parse instantiation, multi-function calls, fixed function calls, and dynamic calls correctly" in {
    /* "let point = Point(1, 5)" --> Stmt.VariableDeclaration(
      "point", false, None,
      Stmt.SimpleCall("Point", Vector(Stmt.IntLiteral(1), Stmt.IntLiteral(5))),
    )
    "let mut position: Position3D = Position3D(5.5, 6.7)" --> Stmt.VariableDeclaration(
      "position", true, Some(Type.Identifier("Position3D")),
      Stmt.SimpleCall("Position3D", Vector(Stmt.RealLiteral(5.5), Stmt.RealLiteral(6.7))),
    )
    "concat('stringA', 'stringB', 'stringC')" --> Stmt.SimpleCall(
      "concat", Vector(Stmt.StringLiteral("stringA"), Stmt.StringLiteral("stringB"), Stmt.StringLiteral("stringC")),
    ) */
    "applyDot.fixed[Dot, Health](dot, e)" --> Stmt.FixedFunctionCall(
      "applyDot", Vector(Type.Identifier("Dot"), Type.Identifier("Health")),
      Vector(Stmt.Variable("dot"), Stmt.Variable("e")),
    )
    "dynamic[String]('readFile', 'file.ext')" --> Stmt.DynamicCall(
      Type.Identifier("String"),
      Vector(
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
    "let x: Int = a" --> Stmt.VariableDeclaration("x", false, Some(Type.Identifier("Int")), va)
    "let mut y: Real = b" --> Stmt.VariableDeclaration("y", true, Some(Type.Identifier("Real")), vb)
    "let z: E & L = c" --> Stmt.VariableDeclaration(
      "z", false,
      Some(Type.Intersection(Vector(Type.Identifier("E"), Type.Identifier("L")))),
      vc,
    )
  }

  it should "parse a block-rich expression within 50 milliseconds" in {
    timed(50) { () =>
      "{ a + { b } + { b }.x + b }" --> Stmt.Block(Vector(
        Stmt.Addition(
          Stmt.Addition(
            Stmt.Addition(va, Stmt.Block(Vector(vb))),
            Stmt.PropertyAccess(Stmt.Block(Vector(vb)), "x"),
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
        Vector.fill(repetitions)(
          Stmt.Addition(
            Stmt.Addition(
              va,
              Stmt.Block(Vector(Stmt.IfElse(
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
}
