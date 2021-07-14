package lore.compiler.syntax

import lore.compiler.core.Position
import lore.compiler.syntax.Node.{NameNode, NamedNode}

/**
  * All top-level declaration nodes.
  */
sealed trait DeclNode extends NamedNode

object DeclNode {
  /**
    * Function declarations. These include action declarations, which are resolved as "syntactic sugar" by the parser.
    *
    * @param body Notably, a function body is NOT a TopLevelExprNode. Rather, it may be a block which can then
    *             represent TopLevelExprNode.
    */
  case class FunctionNode(
    nameNode: NameNode,
    parameters: Vector[ParameterNode],
    outputType: TypeExprNode,
    typeVariables: Vector[TypeVariableNode],
    body: Option[ExprNode],
    isAction: Boolean,
    position: Position,
  ) extends DeclNode {
    def isAbstract: Boolean = body.isEmpty
  }

  object FunctionNode {
    def fromFunction(
      nameNode: NameNode,
      parameters: Vector[ParameterNode],
      outputType: TypeExprNode,
      typeVariables: Vector[TypeVariableNode],
      body: Option[ExprNode],
      position: Position,
    ): FunctionNode = {
      DeclNode.FunctionNode(nameNode, parameters, outputType, typeVariables, body, isAction = false, position)
    }

    def fromAction(
      nameNode: NameNode,
      parameters: Vector[ParameterNode],
      typeVariables: Vector[TypeVariableNode],
      body: Option[ExprNode],
      position: Position,
    ): FunctionNode = {
      DeclNode.FunctionNode(nameNode, parameters, TypeExprNode.UnitNode(position), typeVariables, body, isAction = true, position)
    }
  }

  case class ParameterNode(
    nameNode: NameNode,
    tpe: TypeExprNode,
    position: Position,
  ) extends NamedNode

  case class TypeVariableNode(
    nameNode: NameNode,
    lowerBound: Option[TypeExprNode],
    upperBound: Option[TypeExprNode],
    position: Position,
  ) extends NamedNode
}

/**
  * Top-level type declarations.
  */
sealed trait TypeDeclNode extends DeclNode

object TypeDeclNode {

  case class AliasNode(override val nameNode: NameNode, tpe: TypeExprNode, position: Position) extends TypeDeclNode

  /**
    * @param extended The names of all traits that the struct extends.
    */
  case class StructNode(
    nameNode: NameNode,
    extended: Vector[TypeExprNode],
    properties: Vector[PropertyNode],
    position: Position,
  ) extends TypeDeclNode

  case class PropertyNode(
    nameNode: NameNode,
    tpe: TypeExprNode,
    isOpen: Boolean,
    isMutable: Boolean,
    defaultValue: Option[ExprNode],
    position: Position,
  ) extends NamedNode

  /**
    * @param extended The names of all traits that the trait extends.
    */
  case class TraitNode(
    nameNode: NameNode,
    extended: Vector[TypeExprNode],
    position: Position,
  ) extends TypeDeclNode

}
