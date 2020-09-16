package lore.compiler.syntax

import lore.compiler.core.Position

/**
  * All type expressions.
  */
sealed trait TypeExprNode extends Node
object TypeExprNode {
  case class IdentifierNode(name: String, position: Position) extends TypeExprNode
  case class IntersectionNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class SumNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class ProductNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class UnitNode(position: Position) extends TypeExprNode
  case class ListNode(element: TypeExprNode, position: Position) extends TypeExprNode
  case class MapNode(key: TypeExprNode, value: TypeExprNode, position: Position) extends TypeExprNode
  case class ComponentNode(underlyingName: String, position: Position) extends TypeExprNode
}
