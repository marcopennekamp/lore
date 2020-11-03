package lore.compiler.phases.parsing

import fastparse.P
import lore.compiler.core.Fragment
import lore.compiler.syntax._
import lore.compiler.test.BaseSpec

class FragmentParserSpec extends BaseSpec with ParserSpecExtensions[Vector[DeclNode]] {

  override def parser[_: P](implicit fragment: Fragment): P[Vector[DeclNode]] = new FragmentParser().fullFragment

  import TestNodes._

  it should "parse action declarations correctly" in {
    """
    |  action attack(source: +Arms, target: +Health) {
    |    let power = combinedPower(source.Arms)
    |    damage(target, power)
    |  }
    """.stripMargin --> Vector(Decl.Function(
      "attack",
      Vector(
        Decl.Parameter("source", Type.Component("Arms")),
        Decl.Parameter("target", Type.Component("Health")),
      ),
      Type.Unit(),
      Vector.empty,
      Some(Stmt.Block(Vector(
        Stmt.VariableDeclaration(
          "power", false, None, Stmt.SimpleCall("combinedPower", Vector(Stmt.PropertyAccess(Stmt.Variable("source"), "Arms"))),
        ),
        Stmt.SimpleCall("damage", Vector(Stmt.Variable("target"), Stmt.Variable("power"))),
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

  "The type declaration parser" should "parse struct and property declarations correctly" in {
    """
    |  struct Position { x: Real = 0.0, y: Real = 0.0, z: Real = 0.0 }
    |
    |  function from2D(x: Real, y: Real): Position = {
    |    Position(x, y, 0.0)
    |  }
    |
    |  function from1D(x: Real): Position = {
    |    from2D(x, 0.0)
    |  }
    """.stripMargin --> Vector(
      Decl.Struct(
        "Position",
        Vector.empty,
        None,
        Vector(
          Decl.Property("x", tReal, false, Some(Stmt.RealLiteral(0.0))),
          Decl.Property("y", tReal, false, Some(Stmt.RealLiteral(0.0))),
          Decl.Property("z", tReal, false, Some(Stmt.RealLiteral(0.0))),
        ),
        false,
      ),
      Decl.Function(
        "from2D",
        Vector(
          Decl.Parameter("x", tReal),
          Decl.Parameter("y", tReal),
        ),
        Type.Identifier("Position"),
        Vector.empty,
        Some(Stmt.Block(Vector(Stmt.SimpleCall("Position", Vector(vx, vy, Stmt.RealLiteral(0.0)))))),
      ),
      Decl.Function(
        "from1D",
        Vector(
          Decl.Parameter("x", tReal),
        ),
        Type.Identifier("Position"),
        Vector.empty,
        Some(Stmt.Block(Vector(Stmt.SimpleCall("from2D", Vector(vx, Stmt.RealLiteral(0.0)))))),
      ),
    )
  }

  it should "parse structs, traits, inheritance, ownership, and independence correctly" in {
    """
    |  independent trait L1
    |  independent trait L2
    |  struct P
    |
    |  trait O1 extends L1, L2
    |  struct O2 implements O1
    |
    |  trait A owned by O1 & L1
    |  struct B implements A, L2 owned by O2 & L1 {
    |    mut property1: P
    |    property2: P
    |  }
    """.stripMargin --> Vector(
      Decl.Trait("L1", Vector.empty, Vector.empty, None, true),
      Decl.Trait("L2", Vector.empty, Vector.empty, None, true),
      Decl.Struct("P", Vector.empty, None, Vector.empty, false),
      Decl.Trait("O1", Vector("L1", "L2"), Vector.empty, None, false),
      Decl.Struct("O2", Vector("O1"), None, Vector.empty, false),
      Decl.Trait("A", Vector.empty, Vector.empty, Some(Type.Intersection(Vector(Type.Identifier("O1"), Type.Identifier("L1")))), false),
      Decl.Struct(
        "B",
        Vector("A", "L2"),
        Some(Type.Intersection(Vector(Type.Identifier("O2"), Type.Identifier("L1")))),
        Vector(
          Decl.Property("property1", Type.Identifier("P"), true, None),
          Decl.Property("property2", Type.Identifier("P"), false, None),
        ),
        false,
      ),
    )
  }

  it should "parse entity and component declarations correctly" in {
    """
    |  struct C1 owned by E1
    |  trait D1
    |  struct D2 implements D1
    |
    |  trait E1 extends +C1, L1, +C2, L2
    |
    |  struct E2 implements E1 {
    |    component D2
    |    mut count: Real
    |  }
    """.stripMargin --> Vector(
      Decl.Struct("C1", Vector.empty, Some(Type.Identifier("E1")), Vector.empty, false),
      Decl.Trait("D1", Vector.empty, Vector.empty, None, false),
      Decl.Struct("D2", Vector("D1"), None, Vector.empty, false),
      Decl.Trait("E1", Vector("L1", "L2"), Vector("C1", "C2"), None, false),
      Decl.Struct(
        "E2",
        Vector("E1"),
        None,
        Vector(
          Decl.Component("D2", None),
          Decl.Property("count", Type.Identifier("Real"), true, None),
        ),
        false,
      ),
    )
  }

}
