package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

object TypeParser {
  import LexicalParser.identifier

  // The parser hierarchy, again, implements "operator precedence". Hence parsers also may be ordered differently
  // than they are declared in the AST files.
  def typeExpression[_: P]: P[TypeExprNode] = P(sumType)

  private def sumType[_: P]: P[TypeExprNode] = P(xarySet("|", intersectionType, TypeExprNode.SumNode) | intersectionType)

  private def intersectionType[_: P]: P[TypeExprNode] = P(xarySet("&", mapType, TypeExprNode.IntersectionNode) | mapType)

  private def mapType[_: P]: P[TypeExprNode] = P(binary("->", atom, TypeExprNode.MapNode) | atom)

  private def atom[_: P]: P[TypeExprNode] = P(unitType | productType | listType | componentType | nominalType | enclosedType)

  private def unitType[_: P]: P[TypeExprNode] = P("(" ~ ")").map(_ => TypeExprNode.UnitNode)

  /**
    * The parser for product types doesn't support tuples of length 1 because of the ambiguity with the enclosedType
    * parser. Since length-1 tuple types are generally useless, we think this is fine as is.
    */
  private def productType[_: P]: P[TypeExprNode.ProductNode] = P("(" ~ xaryList(",", typeExpression, TypeExprNode.ProductNode) ~ ")")

  private def listType[_: P]: P[TypeExprNode.ListNode] = P("[" ~ typeExpression ~ "]").map(TypeExprNode.ListNode)

  private def componentType[_: P]: P[TypeExprNode.ComponentNode] = P("+" ~/ nominalType).map(TypeExprNode.ComponentNode)

  private def nominalType[_: P]: P[TypeExprNode.NominalNode] = P(identifier).map(TypeExprNode.NominalNode)

  private def enclosedType[_: P]: P[TypeExprNode] = P("(" ~ typeExpression ~ ")")
}
