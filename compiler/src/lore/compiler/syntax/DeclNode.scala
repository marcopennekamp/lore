package lore.compiler.syntax

import lore.compiler.core.Position

/**
  * All top-level declaration nodes.
  */
sealed trait DeclNode extends Node
object DeclNode {
  /**
    * Function declarations. These include action declarations, which are resolved as "syntactic sugar" by the parser.
    *
    * @param body Notably, a function body is NOT a TopLevelExprNode. Rather, it may be a block which can then
    *             represent TopLevelExprNode.
    */
  case class FunctionNode(
    name: String, parameters: Vector[ParameterNode], outputType: TypeExprNode, typeVariables: Vector[TypeVariableNode],
    body: Option[ExprNode], position: Position,
  ) extends DeclNode {
    def isAbstract: Boolean = body.isEmpty
  }

  object FunctionNode {
    def fromAction(
      name: String, parameters: Vector[ParameterNode], typeVariables: Vector[TypeVariableNode],
      body: Option[ExprNode], position: Position,
    ): FunctionNode = {
      DeclNode.FunctionNode(name, parameters, TypeExprNode.UnitNode(position), typeVariables, body, position)
    }
  }

  case class ParameterNode(name: String, tpe: TypeExprNode, position: Position) extends Node
  case class TypeVariableNode(
    name: String, lowerBound: Option[TypeExprNode], upperBound: Option[TypeExprNode], position: Position,
  ) extends Node
}

/**
  * Top-level type declarations.
  */
sealed trait TypeDeclNode extends DeclNode {
  def name: String
}

object TypeDeclNode {

  case class AliasNode(override val name: String, tpe: TypeExprNode, position: Position) extends TypeDeclNode

  /**
    * @param implemented The names of all traits that the struct implements.
    */
  case class StructNode(
    override val name: String,
    implemented: Vector[TypeExprNode],
    properties: Vector[PropertyNode],
    position: Position
  ) extends TypeDeclNode

  case class PropertyNode(name: String, tpe: TypeExprNode, isMutable: Boolean, defaultValue: Option[ExprNode], position: Position) extends Node

  /**
    * @param extended The names of all traits that the trait extends.
    */
  case class TraitNode(
    override val name: String,
    extended: Vector[TypeExprNode],
    position: Position
  ) extends TypeDeclNode

}
