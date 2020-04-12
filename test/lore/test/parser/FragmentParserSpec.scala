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
        ParameterNode("x", TypeExprNode.NominalNode("Real")),
        ParameterNode("exp", TypeExprNode.NominalNode("Int")),
      ),
      TypeExprNode.NominalNode("Real"),
      Some(BlockNode(List(
        VariableDeclarationNode("e", isMutable = true, None, VariableNode("exp")),
        VariableDeclarationNode("result", isMutable = true, None, RealLiteralNode(1.0)),
        RepeatWhileNode(
          GreaterThanNode(VariableNode("e"), IntLiteralNode(0)),
          BlockNode(List(
            AssignmentNode(AddressNode(List("result")), MultiplicationNode(VariableNode("result"), VariableNode("x"))),
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
}
