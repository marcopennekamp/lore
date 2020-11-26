package lore.compiler.phases.parsing

import fastparse.P
import lore.compiler.core.Fragment
import lore.compiler.syntax._
import lore.compiler.test.BaseSpec

class FragmentParserSpec extends BaseSpec with ParserSpecExtensions[Vector[DeclNode]] {

  override def parser[_: P](implicit fragment: Fragment): P[Vector[DeclNode]] = new FragmentParser().fullFragment

  import TestNodes._

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
