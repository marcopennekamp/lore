package lore.test.parser

import fastparse.P
import lore.ast._
import lore.parser.FragmentParser
import lore.test.BaseSpec

class FragmentParserSpec extends BaseSpec with ParserSpecExtensions[DeclNode] {
  import DeclNode._
  import TypeDeclNode._
  import TopLevelExprNode._
  import ExprNode._

  override def parser[_: P] = FragmentParser.topDeclaration

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
    |""".stripMargin --> FunctionNode(
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
            AssignmentNode(AddressNode(List("result")), MultiplicationNode(VariableNode("result"), vx)),
            AssignmentNode(AddressNode(List("e")), SubtractionNode(VariableNode("e"), IntLiteralNode(1))),
          )),
          deferCheck = false,
        ),
        VariableNode("result"),
      ))),
    )
  }

  it should "parse action declarations correctly" in {
    """
    |  action attack(source: +Arms, target: +Health) {
    |    const power = combinedPower(source.Arms)
    |    damage(target, power)
    |  }
    """.stripMargin --> FunctionNode(
      "attack",
      List(
        ParameterNode("source", TypeExprNode.ComponentNode(TypeExprNode.NominalNode("Arms"))),
        ParameterNode("target", TypeExprNode.ComponentNode(TypeExprNode.NominalNode("Health"))),
      ),
      TypeExprNode.UnitNode,
      Some(BlockNode(List(
        VariableDeclarationNode(
          "power", isMutable = false, None,
          CallNode("combinedPower", None, List(PropertyAccessNode(VariableNode("source"), List("Arms")))),
        ),
        CallNode("damage", None, List(VariableNode("target"), VariableNode("power"))),
      ))),
    )
  }

  it should "set indices to the correct values" in {
    "function add(a: Real, b: Real): Real = a + b".parsed match {
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
    """.stripMargin --> ClassNode(
      "Position", None, None, isAbstract = false,
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
    )
    // TODO: Test abstract class, extends, owned by.
  }

  it should "fail on classes declaring component members" in {

  }

  it should "parse entity and component declarations correctly" in {

  }
}
