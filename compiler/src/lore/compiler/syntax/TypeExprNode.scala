package lore.compiler.syntax

import lore.compiler.core.Position
import lore.compiler.syntax.Node.{NameNode, NamedNode}
import lore.compiler.utils.CollectionExtensions.VectorExtension

/**
  * All type expressions.
  */
sealed trait TypeExprNode extends Node
object TypeExprNode {
  case class TypeNameNode(name: String, position: Position) extends TypeExprNode
  case class InstantiationNode(nameNode: TypeNameNode, arguments: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class SumNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class IntersectionNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class TupleNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class UnitNode(position: Position) extends TypeExprNode
  case class FunctionNode(input: TypeExprNode, output: TypeExprNode, position: Position) extends TypeExprNode
  case class ListNode(element: TypeExprNode, position: Position) extends TypeExprNode
  case class MapNode(key: TypeExprNode, value: TypeExprNode, position: Position) extends TypeExprNode
  case class ShapeNode(properties: Vector[ShapePropertyNode], position: Position) extends TypeExprNode
  case class ShapePropertyNode(nameNode: NameNode, tpe: TypeExprNode, position: Position) extends NamedNode
  case class SymbolNode(name: String, position: Position) extends TypeExprNode

  /**
    * Constructs a right-associative nested function type from the given types.
    */
  def xaryFunction(types: Vector[TypeExprNode], position: Position): TypeExprNode = {
    types.init.foldRight(types.last) {
      case (input, output) => FunctionNode(input, output, Position(position.fragment, input.position.startIndex, output.position.endIndex))
    }
  }

  /**
    * Collects all leaf nodes in a flattened list.
    */
  def leaves(node: TypeExprNode): Vector[TypeExprNode] = node match {
    case InstantiationNode(nameNode, types, _) => leaves(nameNode) ++ types.flatMap(leaves)
    case SumNode(types, _) => types.flatMap(leaves)
    case IntersectionNode(types, _) => types.flatMap(leaves)
    case TupleNode(types, _) => types.flatMap(leaves)
    case FunctionNode(input, output, _) => leaves(input) ++ leaves(output)
    case ListNode(element, _) => leaves(element)
    case MapNode(key, value, _) => leaves(key) ++ leaves(value)
    case ShapeNode(properties, _) => properties.map(_.tpe).flatMap(leaves)
    case _ => Vector(node)
  }

  /**
    * Finds all identifiers mentioned in the type expression.
    */
  def identifiers(node: TypeExprNode): Set[String] = leaves(node).filterType[TypeNameNode].map(_.name).toSet
}
