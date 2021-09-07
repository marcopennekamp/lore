package lore.compiler.parsing

import fastparse._
import lore.compiler.core.{Fragment, Position}
import lore.compiler.parsing.LexicalParser.{identifier, typeIdentifier}
import lore.compiler.syntax._

/**
  * @param whitespace This can be manually specified to disable newlines in whitespace.
  */
class TypeParser(nameParser: NameParser)(implicit fragment: Fragment, whitespace: P[Any] => P[Unit]) {
  import Node._
  import nameParser.name

  def typing[_: P]: P[TypeExprNode] = P(":" ~ typeExpression)

  def typeExpression[_: P]: P[TypeExprNode] = {
    import PrecedenceParser._
    PrecedenceParser.parser(
      operator = StringIn("|", "&", "=>"),
      operand = atom,
      operatorMeta = Map(
        "|" -> XaryOperator[TypeExprNode](1, TypeExprNode.SumNode),
        "&" -> XaryOperator[TypeExprNode](2, TypeExprNode.IntersectionNode),
        "=>" -> XaryOperator[TypeExprNode](3, TypeExprNode.xaryFunction),
      ),
    )
  }

  private def atom[_: P]: P[TypeExprNode] = {
    P(unitType | tupleType | listType | mapType | shapeType | symbolType | instantiation | namedType | enclosedType)
  }

  private def unitType[_: P]: P[TypeExprNode] = P(Index ~ "(" ~ ")" ~ Index).map {
    case (startIndex, endIndex) => TypeExprNode.UnitNode(Position(fragment, startIndex, endIndex))
  }

  /**
    * The parser for tuple types doesn't support 1-tuples with a syntax `(A)`, because this would clash with the
    * `enclosedType` parser. However, we still want to be able to parse function types that work on single tuple
    * arguments. The syntax `(Int, Int) => Int` would create a function type with two arguments, so we need a special
    * syntax. The solution is to parse `((Int, Int))` as a *nested* tuple.
    */
  private def tupleType[_: P]: P[TypeExprNode.TupleNode] = {
    def nested = P("(" ~ tupleType ~ ")").map(Vector(_))
    def tuple = {
      P("(" ~ typeExpression ~ ("," ~ typeExpression).rep(1) ~ ")")
        .map { case (e1, rest) => e1 +: rest.toVector }
    }
    P(Index ~~ (nested | tuple) ~~ Index).map(withPosition(TypeExprNode.TupleNode))
  }

  private def listType[_: P]: P[TypeExprNode.ListNode] = P(Index ~ "[" ~ typeExpression ~ "]" ~ Index).map(withPosition(TypeExprNode.ListNode))

  private def mapType[_: P]: P[TypeExprNode.MapNode] = P(Index ~ "#[" ~ typeExpression ~ "->" ~ typeExpression ~ "]" ~ Index).map(withPosition(TypeExprNode.MapNode))

  private def shapeType[_: P]: P[TypeExprNode.ShapeNode] = {
    def property = P(Index ~ name ~ typing ~ Index).map(withPosition(TypeExprNode.ShapePropertyNode))
    P(Index ~ "%{" ~ property.rep(0, ",").map(_.toVector) ~ "}" ~ Index).map(withPosition(TypeExprNode.ShapeNode))
  }

  private def symbolType[_: P]: P[TypeExprNode.SymbolNode] = P(Index ~ "#" ~ identifier ~ Index).map(withPosition(TypeExprNode.SymbolNode))

  private def instantiation[_: P]: P[TypeExprNode.InstantiationNode] = {
    P(Index ~ namedType ~ "[" ~ typeExpression.rep(1, ",").map(_.toVector) ~ "]" ~ Index).map(withPosition(TypeExprNode.InstantiationNode))
  }

  private def namedType[_: P]: P[TypeExprNode.TypeNameNode] = P(Index ~ typeIdentifier ~ Index).map(withPosition(TypeExprNode.TypeNameNode))

  private def enclosedType[_: P]: P[TypeExprNode] = P("(" ~ typeExpression ~ ")")
}
