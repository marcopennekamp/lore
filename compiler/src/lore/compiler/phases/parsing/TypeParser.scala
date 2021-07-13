package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.core.{Fragment, Position}
import lore.compiler.phases.parsing.LexicalParser.typeIdentifier
import lore.compiler.syntax._

class TypeParser(implicit fragment: Fragment) {
  import LexicalParser.identifier
  import Node._

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
    P(unitType | tupleType | listType | shapeType | symbolType | namedType | enclosedType)
  }

  private def unitType[_: P]: P[TypeExprNode] = P(Index ~ "(" ~ ")" ~ Index).map {
    case (startIndex, endIndex) => TypeExprNode.UnitNode(Position(fragment, startIndex, endIndex))
  }

  /**
    * The parser for tuple types doesn't support tuples of length 1 because of the ambiguity with the enclosedType
    * parser. Since length-1 tuple types are generally useless, we think this is fine as is.
    */
  private def tupleType[_: P]: P[TypeExprNode.TupleNode] = {
    P(Index ~ "(" ~ typeExpression ~ ("," ~ typeExpression).rep(1) ~ ")" ~ Index).map {
      case (startIndex, e, es, endIndex) => TypeExprNode.TupleNode(e +: es.toVector, Position(fragment, startIndex, endIndex))
    }
  }

  private def listType[_: P]: P[TypeExprNode.ListNode] = P(Index ~ "[" ~ typeExpression ~ "]" ~ Index).map(withPosition(TypeExprNode.ListNode))

  private def shapeType[_: P]: P[TypeExprNode.ShapeNode] = {
    def property = P(Index ~ identifier ~ typing ~ Index).map(withPosition(TypeExprNode.ShapePropertyNode))
    P(Index ~ "{" ~ property.rep(0, ",").map(_.toVector) ~ "}" ~ Index).map(withPosition(TypeExprNode.ShapeNode))
  }

  private def symbolType[_: P]: P[TypeExprNode.SymbolNode] = P(Index ~ "#" ~ identifier ~ Index).map(withPosition(TypeExprNode.SymbolNode))

  private def namedType[_: P]: P[TypeExprNode.IdentifierNode] = P(Index ~ typeIdentifier ~ Index).map(withPosition(TypeExprNode.IdentifierNode))

  private def enclosedType[_: P]: P[TypeExprNode] = P("(" ~ typeExpression ~ ")")
}
