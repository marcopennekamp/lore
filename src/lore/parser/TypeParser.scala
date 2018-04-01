package lore.parser

import lore.ast._

object TypeParser extends IgnoreWhitespace {
  import whitespaceApi._
  import fastparse.noApi._
  import IdentifierParser.identifier

  val sumType: P[SumTypeExpression] = {
    val innerTypeExpression = P(intersectionType | tupleType | typeVariable | enclosedType)
    P(identifier ~ ("|" ~/ innerTypeExpression).rep(min = 1)).map { case (firstType, types) =>
      SumTypeExpression((TypeVariable(firstType) +: types).toSet)
    }
  }

  val intersectionType: P[IntersectionTypeExpression] = {
    val innerTypeExpression = P(tupleType | typeVariable | enclosedType)
    P(identifier ~ ("&" ~ innerTypeExpression).rep(min = 1)).map { case (firstType, types) =>
      IntersectionTypeExpression((TypeVariable(firstType) +: types).toSet)
    }
  }

  val tupleType: P[TupleTypeExpression] = {
    P("(" ~ typeExpression ~ ("," ~ typeExpression).rep ~ ")").map { case (firstType, restTypes) =>
      TupleTypeExpression(firstType +: restTypes)
    }
  }

  val typeVariable: P[TypeVariable] = P(identifier).map(TypeVariable)

  val enclosedType: P[TypeExpression] = P("(" ~ typeExpression ~ ")")

  val typeExpression: P[TypeExpression] = P(sumType | intersectionType | tupleType | typeVariable | enclosedType)
}
