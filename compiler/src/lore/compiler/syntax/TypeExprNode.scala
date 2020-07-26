package lore.compiler.syntax

/**
  * All type expressions.
  */
sealed trait TypeExprNode extends Node
object TypeExprNode {
  /**
    * Just a type name suspended in the aether.
    */
  case class NominalNode(name: String, state: Node.State = new Node.DefaultState) extends TypeExprNode
  // TODO: Maybe just rename to TypeNameNode...

  case class IntersectionNode(types: List[TypeExprNode], state: Node.State = new Node.DefaultState) extends TypeExprNode
  case class SumNode(types: List[TypeExprNode], state: Node.State = new Node.DefaultState) extends TypeExprNode
  case class ProductNode(types: List[TypeExprNode], state: Node.State = new Node.DefaultState) extends TypeExprNode
  case class UnitNode(state: Node.State = new Node.DefaultState) extends TypeExprNode
  case class ListNode(element: TypeExprNode, state: Node.State = new Node.DefaultState) extends TypeExprNode
  case class MapNode(key: TypeExprNode, value: TypeExprNode, state: Node.State = new Node.DefaultState) extends TypeExprNode
  case class ComponentNode(underlyingName: String, state: Node.State = new Node.DefaultState) extends TypeExprNode
}
