package lore.compiler.parsing

import fastparse._
import lore.compiler.core.{Fragment, Position}
import lore.compiler.parsing.LexicalParser.identifier
import lore.compiler.syntax._

/**
  * @param whitespace This can be manually specified to disable newlines in whitespace.
  */
class TypeParser(nameParser: NameParser)(implicit fragment: Fragment, whitespace: P[Any] => P[Unit]) {
  import Node._
  import nameParser._

  def typing[_: P]: P[TypeExprNode] = P(":" ~ typeExpression)

  def typeExpression[_: P]: P[TypeExprNode] = {
    import PrecedenceParser._
    val operatorMeta = Map(
      "|" -> XaryOperator[TypeExprNode](1, TypeExprNode.SumTypeNode),
      "&" -> XaryOperator[TypeExprNode](2, TypeExprNode.IntersectionTypeNode),
      "=>" -> XaryOperator[TypeExprNode](3, TypeExprNode.xaryFunction),
    )
    PrecedenceParser.parser(
      operator = StringIn("|", "&", "=>").!.map(operatorMeta),
      operand = atom,
    )
  }

  private def atom[_: P]: P[TypeExprNode] = {
    P(symbolType | unitType | tupleType | listType | mapType | shapeType | instantiation | namedType | enclosedType)
  }

  private def symbolType[_: P]: P[TypeExprNode.SymbolTypeNode] = P(Index ~~ "#" ~ identifier ~~ Index).map(withPosition(TypeExprNode.SymbolTypeNode))

  private def unitType[_: P]: P[TypeExprNode] = P(Index ~~ "(" ~ ")" ~~ Index).map {
    case (startIndex, endIndex) => TypeExprNode.UnitTypeNode(Position(fragment, startIndex, endIndex))
  }

  /**
    * The parser for tuple types doesn't support 1-tuples with a syntax `(A)`, because this would clash with the
    * `enclosedType` parser. However, we still want to be able to parse function types that work on single tuple
    * arguments. The syntax `(Int, Int) => Int` would create a function type with two arguments, so we need a special
    * syntax. The solution is to parse `((Int, Int))` as a *nested* tuple.
    */
  private def tupleType[_: P]: P[TypeExprNode.TupleTypeNode] = {
    def nested = P("(" ~ tupleType ~ ")").map(Vector(_))
    def tuple = {
      P("(" ~ typeExpression ~ ("," ~ typeExpression).rep(1) ~ ",".? ~ ")")
        .map { case (e1, rest) => e1 +: rest.toVector }
    }
    P(Index ~~ (nested | tuple) ~~ Index).map(withPosition(TypeExprNode.TupleTypeNode))
  }

  private def listType[_: P]: P[TypeExprNode.ListTypeNode] = P(Index ~~ "[" ~ typeExpression ~ "]" ~~ Index).map(withPosition(TypeExprNode.ListTypeNode))

  private def mapType[_: P]: P[TypeExprNode.MapTypeNode] = P(Index ~~ "#[" ~ typeExpression ~ "->" ~ typeExpression ~ "]" ~~ Index).map(withPosition(TypeExprNode.MapTypeNode))

  private def shapeType[_: P]: P[TypeExprNode.ShapeTypeNode] = {
    def property = P(Index ~~ name ~ typing ~~ Index).map(withPosition(TypeExprNode.ShapeTypePropertyNode))
    P(Index ~~ "%{" ~ property.rep(0, ",").map(_.toVector) ~ ",".? ~ "}" ~~ Index).map(withPosition(TypeExprNode.ShapeTypeNode))
  }

  private def instantiation[_: P]: P[TypeExprNode.InstantiatedTypeNode] = {
    P(Index ~~ namedType ~ "[" ~ typeExpression.rep(1, ",").map(_.toVector) ~ ",".? ~ "]" ~~ Index).map(withPosition(TypeExprNode.InstantiatedTypeNode))
  }

  private def namedType[_: P]: P[TypeExprNode.TypeNameNode] = P(Index ~~ typeNamePath ~~ Index).map(withPosition(TypeExprNode.TypeNameNode))

  private def enclosedType[_: P]: P[TypeExprNode] = P("(" ~ typeExpression ~ ")")
}
