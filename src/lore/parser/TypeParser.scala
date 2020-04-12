package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

object TypeParser {
  import LexicalParser.identifier

  def typing[_: P]: P[TypeExprNode] = P(":" ~ typeExpression)

  def typeExpression[_: P]: P[TypeExprNode] = {
    import PrecedenceParser._
    PrecedenceParser.parser(
      operator = StringIn("|", "&", "->"),
      operand = atom,
      operatorMeta = Map(
        "|" -> XaryOperator(1, (list: List[TypeExprNode]) => TypeExprNode.SumNode(list.toSet)),
        "&" -> XaryOperator(2, (list: List[TypeExprNode]) => TypeExprNode.IntersectionNode(list.toSet)),
        "->" -> BinaryOperator(3, TypeExprNode.MapNode),
      ),
    )
  }

  private def atom[_: P]: P[TypeExprNode] = P(unitType | productType | listType | componentType | nominalType | enclosedType)

  private def unitType[_: P]: P[TypeExprNode] = P("(" ~ ")").map(_ => TypeExprNode.UnitNode)

  /**
    * The parser for product types doesn't support tuples of length 1 because of the ambiguity with the enclosedType
    * parser. Since length-1 tuple types are generally useless, we think this is fine as is.
    */
  private def productType[_: P]: P[TypeExprNode.ProductNode] = {
    P("(" ~ typeExpression ~ ("," ~ typeExpression).rep(1) ~ ")").map { case (e, es) => TypeExprNode.ProductNode(e +: es.toList) }
  }

  private def listType[_: P]: P[TypeExprNode.ListNode] = P("[" ~ typeExpression ~ "]").map(TypeExprNode.ListNode)

  private def componentType[_: P]: P[TypeExprNode.ComponentNode] = P("+" ~ nominalType).map(TypeExprNode.ComponentNode)

  private def nominalType[_: P]: P[TypeExprNode.NominalNode] = P(identifier).map(TypeExprNode.NominalNode)

  private def enclosedType[_: P]: P[TypeExprNode] = P("(" ~ typeExpression ~ ")")
}
