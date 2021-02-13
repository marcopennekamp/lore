package lore.compiler.syntax

import lore.compiler.core.Position

/**
  * All type expressions.
  */
sealed trait TypeExprNode extends Node
object TypeExprNode {
  case class IdentifierNode(name: String, position: Position) extends TypeExprNode
  case class SumNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class IntersectionNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class ProductNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class UnitNode(position: Position) extends TypeExprNode
  case class FunctionNode(input: TypeExprNode, output: TypeExprNode, position: Position) extends TypeExprNode
  case class ListNode(element: TypeExprNode, position: Position) extends TypeExprNode
  case class MapNode(key: TypeExprNode, value: TypeExprNode, position: Position) extends TypeExprNode
  case class ShapeNode(properties: Vector[ShapePropertyNode], position: Position) extends TypeExprNode
  case class ShapePropertyNode(name: String, tpe: TypeExprNode, position: Position) extends Node

  /**
    * Finds all identifiers mentioned in the type expression.
    */
  def identifiers(expr: TypeExprNode): Set[String] = expr match {
    case IdentifierNode(name, _) => Set(name)
    case SumNode(types, _) => types.flatMap(identifiers).toSet
    case IntersectionNode(types, _) => types.flatMap(identifiers).toSet
    case ProductNode(types, _) => types.flatMap(identifiers).toSet
    case FunctionNode(input, output, _) => identifiers(input) ++ identifiers(output)
    case ListNode(element, _) => identifiers(element)
    case MapNode(key, value, _) => identifiers(key) ++ identifiers(value)
    case ShapeNode(properties, _) => properties.map(_.tpe).flatMap(identifiers).toSet
    case _ => Set.empty
  }
}
