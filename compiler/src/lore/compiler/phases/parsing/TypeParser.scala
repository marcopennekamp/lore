package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.syntax._
import lore.compiler.core.{Fragment, Position}
import lore.compiler.phases.parsing.LexicalParser.typeIdentifier

class TypeParser(implicit fragment: Fragment) {
  import Node._
  import LexicalParser.identifier

  def typing[_: P]: P[TypeExprNode] = P(":" ~ typeExpression)

  def typeExpression[_: P]: P[TypeExprNode] = {
    import PrecedenceParser._
    PrecedenceParser.parser(
      operator = StringIn("|", "&", "=>", "->"),
      operand = atom,
      operatorMeta = Map(
        "|" -> XaryOperator[TypeExprNode](1, TypeExprNode.SumNode),
        "&" -> XaryOperator[TypeExprNode](2, TypeExprNode.IntersectionNode),
        "=>" -> BinaryOperator[TypeExprNode](3, TypeExprNode.FunctionNode),
        "->" -> BinaryOperator[TypeExprNode](4, TypeExprNode.MapNode),
      ),
    )
  }

  private def atom[_: P]: P[TypeExprNode] = {
    P(unitType | productType | listType | shapeType | namedType | enclosedType)
  }

  private def unitType[_: P]: P[TypeExprNode] = P(Index ~ "(" ~ ")").map(index => TypeExprNode.UnitNode(Position(fragment, index)))

  /**
    * The parser for product types doesn't support tuples of length 1 because of the ambiguity with the enclosedType
    * parser. Since length-1 tuple types are generally useless, we think this is fine as is.
    */
  private def productType[_: P]: P[TypeExprNode.ProductNode] = {
    P(Index ~ "(" ~ typeExpression ~ ("," ~ typeExpression).rep(1) ~ ")").map {
      case (index, e, es) => TypeExprNode.ProductNode(e +: es.toVector, Position(fragment, index))
    }
  }

  private def listType[_: P]: P[TypeExprNode.ListNode] = P(Index ~ "[" ~ typeExpression ~ "]").map(withPosition(TypeExprNode.ListNode))

  private def shapeType[_: P]: P[TypeExprNode.ShapeNode] = {
    def property = P(Index ~ identifier ~ typing).map(withPosition(TypeExprNode.ShapePropertyNode))
    P(Index ~ "{" ~ property.rep(0, ",").map(_.toVector) ~ "}").map(withPosition(TypeExprNode.ShapeNode))
  }

  private def namedType[_: P]: P[TypeExprNode.IdentifierNode] = P(Index ~ typeIdentifier).map(withPosition(TypeExprNode.IdentifierNode))

  private def enclosedType[_: P]: P[TypeExprNode] = P("(" ~ typeExpression ~ ")")
}
