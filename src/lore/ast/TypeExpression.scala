package lore.ast

trait TypeExpression
case class TypeVariable(name: String) extends TypeExpression
case class IntersectionType(types: Set[TypeExpression]) extends TypeExpression
case class TupleType(types: Seq[TypeExpression]) extends TypeExpression
