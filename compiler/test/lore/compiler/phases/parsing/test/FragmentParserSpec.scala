package lore.compiler.phases.parsing.test

import fastparse.P
import lore.compiler.syntax._
import lore.compiler.core.{Fragment, Position}
import lore.compiler.phases.parsing.FragmentParser
import lore.compiler.test.BaseSpec

class FragmentParserSpec extends BaseSpec with ParserSpecExtensions[List[DeclNode]] {
  implicit private val fragment: Fragment = Fragment("Test", "")
  override def parser[_: P]: P[List[DeclNode]] = new FragmentParser().fullFragment

  import TestNodes._

  "The function declaration parser" should "parse function declarations correctly" in {
    """
    |  function pow(x: Real, exp: Int): Real = {
    |    let e = exp
    |    let result = 1.0
    |    while (e > 0) {
    |      result *= x
    |      e -= 1
    |    }
    |    result
    |  }
    |""".stripMargin --> List(Decl.Function(
      "pow",
      List(
        Decl.Parameter("x", tReal),
        Decl.Parameter("exp", tInt),
      ),
      tReal,
      Nil,
      Some(Stmt.Block(List(
        Stmt.VariableDeclaration("e", true, None, Stmt.Variable("exp")),
        Stmt.VariableDeclaration("result", true, None, Stmt.RealLiteral(1.0)),
        Stmt.Repetition(
          Stmt.GreaterThan(Stmt.Variable("e"), Stmt.IntLiteral(0)),
          Stmt.Block(List(
            Stmt.Assignment(Stmt.Variable("result"), Stmt.Multiplication(Stmt.Variable("result"), vx)),
            Stmt.Assignment(Stmt.Variable("e"), Stmt.Subtraction(Stmt.Variable("e"), Stmt.IntLiteral(1))),
          )),
        ),
        Stmt.Variable("result"),
      ))),
    ))
  }

  it should "parse action declarations correctly" in {
    """
    |  action attack(source: +Arms, target: +Health) {
    |    const power = combinedPower(source.Arms)
    |    damage(target, power)
    |  }
    """.stripMargin --> List(Decl.Function(
      "attack",
      List(
        Decl.Parameter("source", Type.Component("Arms")),
        Decl.Parameter("target", Type.Component("Health")),
      ),
      Type.Unit(),
      Nil,
      Some(Stmt.Block(List(
        Stmt.VariableDeclaration(
          "power", false, None, Stmt.SimpleCall("combinedPower", None, List(Stmt.PropertyAccess(Stmt.Variable("source"), "Arms"))),
        ),
        Stmt.SimpleCall("damage", None, List(Stmt.Variable("target"), Stmt.Variable("power"))),
      ))),
    ))
  }

  it should "assign the correct indices" in {
    "function add(a: Real, b: Real): Real = a + b".parsed.head match {
      case fn: DeclNode.FunctionNode =>
        fn.position.index shouldEqual 0
        fn.parameters match {
          case Seq(a, b) =>
            a.position.index shouldEqual 13
            b.position.index shouldEqual 22
        }
        fn.outputType.position.index shouldEqual 32
        fn.body.value.position.index shouldEqual 39
    }
  }

  "The type declaration parser" should "parse class, property and constructor declarations correctly" in {
    """
    |  class Position {
    |    x: Real
    |    y: Real
    |    z: Real
    |
    |    Position(x: Real, y: Real, z: Real) {
    |      construct(x, y, z)
    |    }
    |
    |    from2D(x: Real, y: Real) {
    |      // We declare z like this to test whether the parser correctly separates the statement list
    |      // and the continuation at the end.
    |      const z = 0.0
    |      this(x, y, z)
    |    }
    |
    |    from1D(x: Real) {
    |      const y = 0.0
    |      this.from2D(x, y)
    |    }
    |  }
    """.stripMargin --> List(Decl.Class(
      "Position", None, None, false, false,
      List(
        Decl.Property("x", tReal, false),
        Decl.Property("y", tReal, false),
        Decl.Property("z", tReal, false),
      ),
      List(
        Decl.Constructor(
          "Position",
          List(
            Decl.Parameter("x", tReal),
            Decl.Parameter("y", tReal),
            Decl.Parameter("z", tReal),
          ),
          Stmt.Block(List(
            Stmt.Construct(List(vx, vy, vz), None),
          )),
        ),
        Decl.Constructor(
          "from2D",
          List(
            Decl.Parameter("x", tReal),
            Decl.Parameter("y", tReal),
          ),
          Stmt.Block(List(
            Stmt.VariableDeclaration("z", false, None, Stmt.RealLiteral(0.0)),
            Stmt.ConstructorCall(None, false, List(vx, vy, vz)),
          )),
        ),
        Decl.Constructor(
          "from1D",
          List(
            Decl.Parameter("x", tReal),
          ),
          Stmt.Block(List(
            Stmt.VariableDeclaration("y", false, None, Stmt.RealLiteral(0.0)),
            Stmt.ConstructorCall(Some("from2D"), false, List(vx, vy)),
          )),
        )
      ),
    ))
  }

  it should "parse abstractness, inheritance, and ownership correctly" in {
    """
      |  label L
      |
      |  class P
      |  class Q
      |
      |  abstract class O1
      |  class O2 extends O1
      |
      |  abstract class A owned by O1 & L {
      |    property: P
      |  }
      |
      |  class B extends A owned by O2 & L {
      |    mut quoperty: Q
      |  }
    """.stripMargin --> List(
      Decl.Label("L", None),
      Decl.Class("P", None, None, false, false, Nil, Nil),
      Decl.Class("Q", None, None, false, false, Nil, Nil),
      Decl.Class("O1", None, None, true, false, Nil, Nil),
      Decl.Class("O2", Some("O1"), None, false, false, Nil, Nil),
      TypeDeclNode.ClassNode(
        "A",
        supertypeName = None,
        ownedBy = Some(
          Type.Intersection(List(Type.Nominal("O1"), Type.Nominal("L")))
        ),
        isAbstract = true,
        isEntity = false,
        members = List(
          Decl.Property("property", Type.Nominal("P"), false),
        ),
        constructors = Nil,
        Position.Wildcard,
      ),
      TypeDeclNode.ClassNode(
        "B",
        supertypeName = Some("A"),
        ownedBy = Some(
          Type.Intersection(List(Type.Nominal("O2"), Type.Nominal("L")))
        ),
        isAbstract = false,
        isEntity = false,
        members = List(
          Decl.Property("quoperty", Type.Nominal("Q"), true),
        ),
        constructors = Nil,
        Position.Wildcard,
      ),
    )
  }

  it should "fail on classes declaring component members" in {
    """
      |  class C
      |  class D
      |
      |  class E {
      |    component C
      |    component D
      |  }
    """.stripMargin.fails
  }

  it should "parse entity and component declarations correctly" in {
    """
      |  class C1 owned by E1
      |  class D1
      |  class D2 extends D1
      |
      |  abstract entity E1 {
      |    component C1
      |    component D1
      |
      |    E1(c1: C1, d1: D1) {
      |      construct(c1, d1)
      |    }
      |  }
      |
      |  entity E2 extends E1 {
      |    component D2 overrides D1
      |
      |    E2(c1: C1, d2: D2) {
      |      construct(d2) with super(c1, d2)
      |    }
      |  }
    """.stripMargin --> List(
      Decl.Class("C1", None, Some(Type.Nominal("E1")), false, false, Nil, Nil),
      Decl.Class("D1", None, None, false, false, Nil, Nil),
      Decl.Class("D2", Some("D1"), None, false, false, Nil, Nil),
      TypeDeclNode.ClassNode(
        "E1",
        supertypeName = None,
        ownedBy = None,
        isAbstract = true,
        isEntity = true,
        members = List(Decl.Component("C1", None), Decl.Component("D1", None)),
        constructors = List(
          Decl.Constructor(
            "E1",
            List(Decl.Parameter("c1", Type.Nominal("C1")), Decl.Parameter("d1", Type.Nominal("D1"))),
            Stmt.Block(List(
              Stmt.Construct(List(Stmt.Variable("c1"), Stmt.Variable("d1")), None),
            )),
          ),
        ),
        Position.Wildcard,
      ),
      TypeDeclNode.ClassNode(
        "E2",
        supertypeName = Some("E1"),
        ownedBy = None,
        isAbstract = false,
        isEntity = true,
        members = List(Decl.Component("D2", Some("D1"))),
        constructors = List(
          Decl.Constructor(
            "E2",
            List(Decl.Parameter("c1", Type.Nominal("C1")), Decl.Parameter("d2", Type.Nominal("D2"))),
            Stmt.Block(List(
              Stmt.Construct(
                List(Stmt.Variable("d2")),
                Some(Stmt.ConstructorCall(None, true, List(Stmt.Variable("c1"), Stmt.Variable("d2")))),
              ),
            )),
          ),
        ),
        Position.Wildcard,
      ),
    )
  }
}
