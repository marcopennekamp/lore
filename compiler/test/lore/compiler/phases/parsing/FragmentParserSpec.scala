package lore.compiler.phases.parsing

import fastparse.P
import lore.compiler.core.Fragment
import lore.compiler.syntax._
import lore.compiler.test.BaseSpec

class FragmentParserSpec extends BaseSpec with ParserSpecExtensions[Vector[DeclNode]] {

  override def parser[_: P](implicit fragment: Fragment): P[Vector[DeclNode]] = new FragmentParser().fullFragment

  import TestNodes._

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
