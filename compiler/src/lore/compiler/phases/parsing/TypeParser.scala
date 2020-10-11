package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.syntax._
import lore.compiler.core.{Fragment, Position}

class TypeParser(implicit fragment: Fragment) {
  import Node._
  import LexicalParser.identifier

  def typing[_: P]: P[TypeExprNode] = P(":" ~ typeExpression)

  def typeExpression[_: P]: P[TypeExprNode] = {
    import PrecedenceParser._
    PrecedenceParser.parser(
      operator = StringIn("|", "&", "->"),
      operand = atom,
      operatorMeta = Map(
        "|" -> XaryOperator[TypeExprNode](1, TypeExprNode.SumNode),
        "&" -> XaryOperator[TypeExprNode](2, TypeExprNode.IntersectionNode),
        "->" -> BinaryOperator[TypeExprNode](3, TypeExprNode.MapNode),
      ),
    )
  }

  private def atom[_: P]: P[TypeExprNode] = {
    P(unitType | productType | listType | componentType | nominalType | enclosedType)
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

  private def listType[_: P]: P[TypeExprNode.ListNode] = P(Index ~ "[" ~ typeExpression ~ "]").map(withIndex(TypeExprNode.ListNode))

  private def componentType[_: P]: P[TypeExprNode.ComponentNode] = P(Index ~ "+" ~ identifier).map(withIndex(TypeExprNode.ComponentNode))

  /**
    * Parses a nominal type. Assigns the index itself because it's used by componentType.
    */
  private def nominalType[_: P]: P[TypeExprNode.IdentifierNode] = P(Index ~ identifier).map(withIndex(TypeExprNode.IdentifierNode))

  private def enclosedType[_: P]: P[TypeExprNode] = P("(" ~ typeExpression ~ ")")
}
