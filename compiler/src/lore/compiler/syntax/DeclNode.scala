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
      name: String, parameters: List[ParameterNode], typeVariables: List[TypeVariableNode],
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

  /**
    * @param implemented The names of all traits that the struct implements.
    */
  case class StructNode(
    override val name: String,
    implemented: Vector[String],
    // TODO: Owned by.
    members: Vector[MemberNode],
    position: Position
  ) extends TypeDeclNode

  sealed trait MemberNode extends Node
  case class PropertyNode(name: String, tpe: TypeExprNode, isMutable: Boolean, position: Position) extends MemberNode
  case class ComponentNode(name: String, position: Position) extends MemberNode

  /**
    * @param extended The names of all traits that the trait extends.
    * @param components The names of all declared types that the trait should have as components.
    */
  case class TraitNode(
    override val name: String,
    extended: Vector[String],
    components: Vector[String],
    // TODO: Owned by.
    position: Position
  ) extends TypeDeclNode
}
