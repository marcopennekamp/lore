package lore.ast

trait TypeExpression
case class TypeVariable(name: String) extends TypeExpression
case class IntersectionTypeExpression(types: Set[TypeExpression]) extends TypeExpression
case class SumTypeExpression(types: Set[TypeExpression]) extends TypeExpression
case class TupleTypeExpression(types: List[TypeExpression]) extends TypeExpression
