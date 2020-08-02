package lore.compiler.syntax

import lore.compiler.core.Position

/**
  * All type expressions.
  */
sealed trait TypeExprNode extends Node
object TypeExprNode {
  /**
    * Just a type name suspended in the aether.
    */
  case class NominalNode(name: String, position: Position) extends TypeExprNode
  // TODO: Maybe just rename to TypeNameNode...

  case class IntersectionNode(types: List[TypeExprNode], position: Position) extends TypeExprNode
  case class SumNode(types: List[TypeExprNode], position: Position) extends TypeExprNode
  case class ProductNode(types: List[TypeExprNode], position: Position) extends TypeExprNode
  case class UnitNode(position: Position) extends TypeExprNode
  case class ListNode(element: TypeExprNode, position: Position) extends TypeExprNode
  case class MapNode(key: TypeExprNode, value: TypeExprNode, position: Position) extends TypeExprNode
  case class ComponentNode(underlyingName: String, position: Position) extends TypeExprNode
}
