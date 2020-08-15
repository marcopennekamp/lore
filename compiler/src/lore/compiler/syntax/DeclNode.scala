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
    name: String, parameters: List[ParameterNode], outputType: TypeExprNode, typeVariables: List[TypeVariableNode],
    body: Option[ExprNode], position: Position,
  ) extends DeclNode {
    def isAbstract: Boolean = body.isEmpty
  }

  object FunctionNode {
    def fromAction(
      name: String, parameters: List[ParameterNode], typeVariables: List[TypeVariableNode], body: Option[ExprNode], position: Position,
    ): FunctionNode = {
      DeclNode.FunctionNode(name, parameters, TypeExprNode.UnitNode(position), typeVariables, body, position)
    }
  }

  case class ParameterNode(name: String, tpe: TypeExprNode, position: Position) extends Node
  case class TypeVariableNode(name: String, lowerBound: Option[TypeExprNode], upperBound: Option[TypeExprNode], position: Position) extends Node
}

/**
  * Top-level type declarations.
  */
sealed trait TypeDeclNode extends DeclNode {
  def name: String
}

object TypeDeclNode {
  case class AliasNode(override val name: String, tpe: TypeExprNode, position: Position) extends TypeDeclNode

  sealed trait DeclaredNode extends TypeDeclNode {
    def supertypeName: Option[String]
  }

  case class LabelNode(override val name: String, override val supertypeName: Option[String], position: Position) extends DeclaredNode

  /**
    * Either a class or an entity depending on whether members contains a component.
    */
  case class ClassNode(
    override val name: String, override val supertypeName: Option[String], ownedBy: Option[TypeExprNode],
    isAbstract: Boolean, isEntity: Boolean, members: List[MemberNode], constructors: List[ConstructorNode],
    position: Position,
  ) extends DeclaredNode

  sealed trait MemberNode extends Node
  case class PropertyNode(name: String, tpe: TypeExprNode, isMutable: Boolean, position: Position) extends MemberNode
  case class ComponentNode(name: String, overrides: Option[String], position: Position) extends MemberNode

  /**
    * This node is the default constructor if its name equals the name of the class it belongs to.
    *
    * The node is only valid if the last statement of the body is a continuation node.
    */
  case class ConstructorNode(
    name: String, parameters: List[DeclNode.ParameterNode], body: ExprNode.BlockNode, position: Position,
  ) extends Node
}