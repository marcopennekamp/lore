package lore.compiler.phases.parsing.test

import fastparse.P
import lore.ast._
import lore.compiler.phases.parsing.FragmentParser
import lore.test.BaseSpec

class FragmentParserSpec extends BaseSpec with ParserSpecExtensions[List[DeclNode]] {
  import DeclNode._
  import ExprNode._
  import TopLevelExprNode._
  import TypeDeclNode._

  override def parser[_: P] = FragmentParser.fragment

  import TestNodes._

  "The function declaration parser" should "parse function declarations correctly" in {
    """
    |  function pow(x: Real, exp: Int): Real = {
    |    let e = exp
    |    let result = 1.0
    |    repeat while (e > 0) {
    |      result *= x
    |      e -= 1
    |    }
    |    result
    |  }
    |""".stripMargin --> List(FunctionNode(
      "pow",
      List(
        ParameterNode("x", tReal),
        ParameterNode("exp", tInt),
      ),
      tReal,
      Some(BlockNode(List(
        VariableDeclarationNode("e", isMutable = true, None, VariableNode("exp")),
        VariableDeclarationNode("result", isMutable = true, None, RealLiteralNode(1.0)),
        RepeatWhileNode(
          GreaterThanNode(VariableNode("e"), IntLiteralNode(0)),
          BlockNode(List(
            AssignmentNode(VariableNode("result"), MultiplicationNode(VariableNode("result"), vx)),
            AssignmentNode(VariableNode("e"), SubtractionNode(VariableNode("e"), IntLiteralNode(1))),
          )),
          deferCheck = false,
        ),
        VariableNode("result"),
      ))),
    ))
  }

  it should "parse action declarations correctly" in {
    """
    |  action attack(source: +Arms, target: +Health) {
    |    const power = combinedPower(source.Arms)
    |    damage(target, power)
    |  }
    """.stripMargin --> List(FunctionNode(
      "attack",
      List(
        ParameterNode("source", TypeExprNode.ComponentNode("Arms")),
        ParameterNode("target", TypeExprNode.ComponentNode("Health")),
      ),
      TypeExprNode.UnitNode,
      Some(BlockNode(List(
        VariableDeclarationNode(
          "power", isMutable = false, None,
          SimpleCallNode("combinedPower", None, List(PropertyAccessNode(VariableNode("source"), "Arms"))),
        ),
        SimpleCallNode("damage", None, List(VariableNode("target"), VariableNode("power"))),
      ))),
    ))
  }

  it should "assign the correct indices" in {
    "function add(a: Real, b: Real): Real = a + b".parsed.head match {
      case fn: FunctionNode =>
        fn.index shouldEqual 0
        fn.parameters match {
          case Seq(a, b) =>
            a.index shouldEqual 13
            b.index shouldEqual 22
        }
        fn.outputType.index shouldEqual 32
        fn.body.value.index shouldEqual 39
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
    """.stripMargin --> List(ClassNode(
      "Position", None, None, isAbstract = false, isEntity = false,
      List(
        PropertyNode("x", tReal, isMutable = false),
        PropertyNode("y", tReal, isMutable = false),
        PropertyNode("z", tReal, isMutable = false),
      ),
      List(
        ConstructorNode(
          "Position",
          List(
            ParameterNode("x", tReal),
            ParameterNode("y", tReal),
            ParameterNode("z", tReal),
          ),
          BlockNode(List(
            ConstructNode(List(vx, vy, vz), None),
          )),
        ),
        ConstructorNode(
          "from2D",
          List(
            ParameterNode("x", tReal),
            ParameterNode("y", tReal),
          ),
          BlockNode(List(
            VariableDeclarationNode("z", isMutable = false, None, RealLiteralNode(0.0)),
            ConstructorCallNode(None, List(vx, vy, vz)),
          )),
        ),
        ConstructorNode(
          "from1D",
          List(
            ParameterNode("x", tReal),
          ),
          BlockNode(List(
            VariableDeclarationNode("y", isMutable = false, None, RealLiteralNode(0.0)),
            ConstructorCallNode(Some("from2D"), List(vx, vy)),
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
      LabelNode("L", None),
      ClassNode("P", None, None, isAbstract = false, isEntity = false, Nil, Nil),
      ClassNode("Q", None, None, isAbstract = false, isEntity = false, Nil, Nil),
      ClassNode("O1", None, None, isAbstract = true, isEntity = false, Nil, Nil),
      ClassNode("O2", Some("O1"), None, isAbstract = false, isEntity = false, Nil, Nil),
      ClassNode(
        "A",
        supertypeName = None,
        ownedBy = Some(
          TypeExprNode.IntersectionNode(List(TypeExprNode.NominalNode("O1"), TypeExprNode.NominalNode("L")))
        ),
        isAbstract = true,
        isEntity = false,
        members = List(
          PropertyNode("property", TypeExprNode.NominalNode("P"), isMutable = false),
        ),
        constructors = Nil,
      ),
      ClassNode(
        "B",
        supertypeName = Some("A"),
        ownedBy = Some(
          TypeExprNode.IntersectionNode(List(TypeExprNode.NominalNode("O2"), TypeExprNode.NominalNode("L")))
        ),
        isAbstract = false,
        isEntity = false,
        members = List(
          PropertyNode("quoperty", TypeExprNode.NominalNode("Q"), isMutable = true),
        ),
        constructors = Nil,
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
      ClassNode("C1", None, ownedBy = Some(TypeExprNode.NominalNode("E1")), isAbstract = false, isEntity = false, Nil, Nil),
      ClassNode("D1", None, None, isAbstract = false, isEntity = false, Nil, Nil),
      ClassNode("D2", supertypeName = Some("D1"), None, isAbstract = false, isEntity = false, Nil, Nil),
      ClassNode(
        "E1",
        supertypeName = None,
        ownedBy = None,
        isAbstract = true,
        isEntity = true,
        members = List(ComponentNode("C1", None), ComponentNode("D1", None)),
        constructors = List(
          ConstructorNode(
            "E1",
            List(ParameterNode("c1", TypeExprNode.NominalNode("C1")), ParameterNode("d1", TypeExprNode.NominalNode("D1"))),
            BlockNode(List(
              ConstructNode(List(VariableNode("c1"), VariableNode("d1")), withSuper = None),
            )),
          ),
        ),
      ),
      ClassNode(
        "E2",
        supertypeName = Some("E1"),
        ownedBy = None,
        isAbstract = false,
        isEntity = true,
        members = List(ComponentNode("D2", overrides = Some("D1"))),
        constructors = List(
          ConstructorNode(
            "E2",
            List(ParameterNode("c1", TypeExprNode.NominalNode("C1")), ParameterNode("d2", TypeExprNode.NominalNode("D2"))),
            BlockNode(List(
              ConstructNode(
                arguments = List(VariableNode("d2")),
                withSuper = Some(ConstructorCallNode(
                  None, List(VariableNode("c1"), VariableNode("d2"))
                )),
              ),
            )),
          ),
        ),
      ),
    )
  }
}
