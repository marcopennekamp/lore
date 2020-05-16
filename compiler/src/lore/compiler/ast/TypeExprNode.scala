package lore.compiler.ast

/**
  * All type expressions.
  */
sealed trait TypeExprNode extends Node
object TypeExprNode {
  /**
    * Just a type name suspended in the aether.
    */
  case class NominalNode(name: String) extends TypeExprNode
  // TODO: Maybe just rename to TypeNameNode...

  case class IntersectionNode(types: List[TypeExprNode]) extends TypeExprNode
  case class SumNode(types: List[TypeExprNode]) extends TypeExprNode
  case class ProductNode(types: List[TypeExprNode]) extends TypeExprNode
  case object UnitNode extends TypeExprNode
  case class ListNode(element: TypeExprNode) extends TypeExprNode
  case class MapNode(key: TypeExprNode, value: TypeExprNode) extends TypeExprNode
  case class ComponentNode(underlyingName: String) extends TypeExprNode
}
