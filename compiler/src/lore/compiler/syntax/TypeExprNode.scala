package lore.compiler.syntax

import lore.compiler.core.Position
import lore.compiler.syntax.Node.{NameNode, NamePathNode, NamedNode, PathNamedNode}

/**
  * All type expressions.
  */
sealed trait TypeExprNode extends Node
object TypeExprNode {
  case class TypeNameNode(namePathNode: NamePathNode, position: Position) extends TypeExprNode with PathNamedNode
  case class InstantiatedTypeNode(typeNameNode: TypeNameNode, arguments: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class SymbolTypeNode(name: String, position: Position) extends TypeExprNode
  case class SumTypeNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class IntersectionTypeNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  case class TupleTypeNode(types: Vector[TypeExprNode], position: Position) extends TypeExprNode
  // TODO (syntax): Why does this exist?
  case class UnitTypeNode(position: Position) extends TypeExprNode
  case class FunctionTypeNode(input: TypeExprNode, output: TypeExprNode, position: Position) extends TypeExprNode
  case class ListTypeNode(element: TypeExprNode, position: Position) extends TypeExprNode
  case class MapTypeNode(key: TypeExprNode, value: TypeExprNode, position: Position) extends TypeExprNode
  case class ShapeTypeNode(properties: Vector[ShapeTypePropertyNode], position: Position) extends TypeExprNode
  case class ShapeTypePropertyNode(nameNode: NameNode, tpe: TypeExprNode, position: Position) extends NamedNode

  /**
    * Constructs a right-associative nested function type from the given types.
    */
  def xaryFunction(types: Vector[TypeExprNode], position: Position): TypeExprNode = {
    types.init.foldRight(types.last) {
      case (input, output) => FunctionTypeNode(input, output, Position(position.fragment, input.position.startIndex, output.position.endIndex))
    }
  }

  /**
    * Collects all leaf nodes in a flattened list.
    */
  def leaves(node: TypeExprNode): Vector[TypeExprNode] = node match {
    case InstantiatedTypeNode(nameNode, types, _) => leaves(nameNode) ++ types.flatMap(leaves)
    case SumTypeNode(types, _) => types.flatMap(leaves)
    case IntersectionTypeNode(types, _) => types.flatMap(leaves)
    case TupleTypeNode(types, _) => types.flatMap(leaves)
    case FunctionTypeNode(input, output, _) => leaves(input) ++ leaves(output)
    case ListTypeNode(element, _) => leaves(element)
    case MapTypeNode(key, value, _) => leaves(key) ++ leaves(value)
    case ShapeTypeNode(properties, _) => properties.map(_.tpe).flatMap(leaves)
    case _ => Vector(node)
  }
}
