package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.ast._
import lore.compiler.core.Fragment

class TypeParser(implicit fragment: Fragment) {
  import Node._
  import LexicalParser.identifier

  def typing[_: P]: P[TypeExprNode] = P(":" ~ typeExpression)

  def typeExpression[_: P]: P[TypeExprNode] = {
    import PrecedenceParser._
    P(Index ~ PrecedenceParser.parser(
      operator = StringIn("|", "&", "->"),
      operand = atom,
      operatorMeta = Map(
        "|" -> XaryOperator[TypeExprNode](1, TypeExprNode.SumNode(_)),
        "&" -> XaryOperator[TypeExprNode](2, TypeExprNode.IntersectionNode(_)),
        "->" -> BinaryOperator[TypeExprNode](3, TypeExprNode.MapNode(_, _)),
      ),
    )).map(withIndex(identity _))
  }

  private def atom[_: P]: P[TypeExprNode] = {
    P(Index ~ (unitType | productType | listType | componentType | nominalType | enclosedType)).map(withIndex(identity _))
  }

  private def unitType[_: P]: P[TypeExprNode] = P("(" ~ ")").map(_ => TypeExprNode.UnitNode())

  /**
    * The parser for product types doesn't support tuples of length 1 because of the ambiguity with the enclosedType
    * parser. Since length-1 tuple types are generally useless, we think this is fine as is.
    */
  private def productType[_: P]: P[TypeExprNode.ProductNode] = {
    P("(" ~ typeExpression ~ ("," ~ typeExpression).rep(1) ~ ")").map { case (e, es) => TypeExprNode.ProductNode(e +: es.toList) }
  }

  private def listType[_: P]: P[TypeExprNode.ListNode] = P("[" ~ typeExpression ~ "]").map(TypeExprNode.ListNode(_))

  private def componentType[_: P]: P[TypeExprNode.ComponentNode] = P("+" ~ identifier).map(TypeExprNode.ComponentNode(_))

  /**
    * Parses a nominal type. Assigns the index itself because it's used by componentType.
    */
  private def nominalType[_: P]: P[TypeExprNode.NominalNode] = P(Index ~ identifier).map(withIndex(TypeExprNode.NominalNode(_)))

  private def enclosedType[_: P]: P[TypeExprNode] = P("(" ~ typeExpression ~ ")")
}
