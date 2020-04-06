package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

object TypeParser {
  import IdentifierParser.identifier

  def sumType[_ : P]: P[SumTypeExpression] = {
    def innerTypeExpression = P(intersectionType | productType | typeVariable | enclosedType)
    P(innerTypeExpression ~ ("|" ~ innerTypeExpression).rep(1)).map { case (firstType, types) =>
      SumTypeExpression((firstType +: types).toSet)
    }
  }

  def intersectionType[_ : P]: P[IntersectionTypeExpression] = {
    def innerTypeExpression = P(productType | typeVariable | enclosedType)
    P(innerTypeExpression ~ ("&" ~ innerTypeExpression).rep(1)).map { case (firstType, types) =>
      IntersectionTypeExpression((firstType +: types).toSet)
    }
  }

  def productType[_ : P]: P[ProductTypeExpression] = {
    P("(" ~ typeExpression ~ ("," ~ typeExpression).rep(1) ~ ")").map { case (firstType, restTypes) =>
      ProductTypeExpression(firstType +: restTypes.toList)
    }
  }

  def typeVariable[_ : P]: P[TypeVariable] = P(identifier).map(TypeVariable)

  def enclosedType[_ : P]: P[TypeExpression] = P("(" ~ typeExpression ~ ")")

  def typeExpression[_ : P]: P[TypeExpression] = P(sumType | intersectionType | productType | typeVariable | enclosedType)
}
