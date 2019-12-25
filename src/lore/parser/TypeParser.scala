package lore.parser

import lore.ast._
import fastparse._

object TypeParser extends IgnoreWhitespace {
  import IdentifierParser.identifier

  def sumType[_ : P]: P[SumTypeExpression] = {
    def innerTypeExpression = P(intersectionType | tupleType | typeVariable | enclosedType)
    P(innerTypeExpression ~ ("|" ~ innerTypeExpression).rep(1)).map { case (firstType, types) =>
      SumTypeExpression((firstType +: types).toSet)
    }
  }

  def intersectionType[_ : P]: P[IntersectionTypeExpression] = {
    def innerTypeExpression = P(tupleType | typeVariable | enclosedType)
    P(innerTypeExpression ~ ("&" ~ innerTypeExpression).rep(1)).map { case (firstType, types) =>
      IntersectionTypeExpression((firstType +: types).toSet)
    }
  }

  def tupleType[_ : P]: P[TupleTypeExpression] = {
    P("(" ~ typeExpression ~ ("," ~ typeExpression).rep(1) ~ ")").map { case (firstType, restTypes) =>
      TupleTypeExpression(firstType +: restTypes)
    }
  }

  def typeVariable[_ : P]: P[TypeVariable] = P(identifier).map(TypeVariable)

  def enclosedType[_ : P]: P[TypeExpression] = P("(" ~ typeExpression ~ ")")

  def typeExpression[_ : P]: P[TypeExpression] = P(sumType | intersectionType | tupleType | typeVariable | enclosedType)
}
