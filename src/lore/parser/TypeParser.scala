package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

object TypeParser {
  import IdentifierParser.identifier

  def sumType[_ : P]: P[TypeExprNode.SumNode] = {
    def innerTypeExpression = P(intersectionType | productType | typeVariable | enclosedType)
    P(innerTypeExpression ~ ("|" ~ innerTypeExpression).rep(1)).map { case (firstType, types) =>
      TypeExprNode.SumNode((firstType +: types).toSet)
    }
  }

  def intersectionType[_ : P]: P[TypeExprNode.IntersectionNode] = {
    def innerTypeExpression = P(productType | typeVariable | enclosedType)
    P(innerTypeExpression ~ ("&" ~ innerTypeExpression).rep(1)).map { case (firstType, types) =>
      TypeExprNode.IntersectionNode((firstType +: types).toSet)
    }
  }

  def productType[_ : P]: P[TypeExprNode.ProductNode] = {
    P("(" ~ typeExpression ~ ("," ~ typeExpression).rep(1) ~ ")").map { case (firstType, restTypes) =>
      TypeExprNode.ProductNode(firstType +: restTypes.toList)
    }
  }

  def typeVariable[_ : P]: P[TypeExprNode.NominalNode] = P(identifier).map(TypeExprNode.NominalNode)

  def enclosedType[_ : P]: P[TypeExprNode] = P("(" ~ typeExpression ~ ")")

  def typeExpression[_ : P]: P[TypeExprNode] = P(sumType | intersectionType | productType | typeVariable | enclosedType)
}
