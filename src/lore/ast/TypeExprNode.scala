package lore.ast

/**
  * All type expressions.
  */
sealed trait TypeExprNode extends Node
object TypeExprNode {
  /**
    * Just a type name suspended in the aether.
    */
  case class NominalNode(name: String) extends TypeExprNode

  case class IntersectionNode(types: Set[TypeExprNode]) extends TypeExprNode
  case class SumNode(types: Set[TypeExprNode]) extends TypeExprNode
  case class ProductNode(types: List[TypeExprNode]) extends TypeExprNode
  case object UnitNode extends TypeExprNode
  case class ListNode(element: TypeExprNode) extends TypeExprNode
  case class MapNode(key: TypeExprNode, value: TypeExprNode) extends TypeExprNode
  case class ComponentNode(underlying: TypeExprNode) extends TypeExprNode
}
