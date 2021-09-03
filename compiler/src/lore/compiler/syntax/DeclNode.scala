package lore.compiler.syntax

import lore.compiler.core.Position
import lore.compiler.syntax.DeclNode.TypeVariableNode
import lore.compiler.syntax.Node.{NameNode, NamedNode}
import lore.compiler.types.TypeVariable.Variance

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
    position: Position,
  ) extends DeclNode {
    def isAbstract: Boolean = body.isEmpty
  }

  object FunctionNode {
    def fromFunction(
      typeVariables: Vector[TypeVariableNode],
      nameNode: NameNode,
      parameters: Vector[ParameterNode],
      outputType: TypeExprNode,
      body: Option[ExprNode],
      position: Position,
    ): FunctionNode = {
      DeclNode.FunctionNode(nameNode, parameters, outputType, typeVariables, body, position)
    }

    def fromAction(
      typeVariables: Vector[TypeVariableNode],
      nameNode: NameNode,
      parameters: Vector[ParameterNode],
      body: Option[ExprNode],
      position: Position,
    ): FunctionNode = {
      DeclNode.FunctionNode(nameNode, parameters, TypeExprNode.UnitNode(position), typeVariables, body, position)
    }
  }

  case class ParameterNode(
    nameNode: Option[NameNode],
    tpe: TypeExprNode,
    position: Position,
  ) extends Node {
    def name: Option[String] = nameNode.map(_.value)
  }

  case class TypeVariableNode(
    nameNode: NameNode,
    lowerBound: Option[TypeExprNode],
    upperBound: Option[TypeExprNode],
    variance: Variance,
    isOpen: Boolean,
    position: Position,
  ) extends NamedNode

  object TypeVariableNode {
    def variant(
      nameNode: NameNode,
      lowerBound: Option[TypeExprNode],
      upperBound: Option[TypeExprNode],
      variance: Variance,
      position: Position,
    ): TypeVariableNode = {
      TypeVariableNode(nameNode, lowerBound, upperBound, variance, isOpen = false, position)
    }

    def simple(
      nameNode: NameNode,
      lowerBound: Option[TypeExprNode],
      upperBound: Option[TypeExprNode],
      position: Position,
    ): TypeVariableNode = {
      variant(nameNode, lowerBound, upperBound, Variance.Invariant, position)
    }
  }
}

/**
  * Top-level type declarations.
  */
sealed trait TypeDeclNode extends DeclNode {
  def typeVariables: Vector[TypeVariableNode]
}

object TypeDeclNode {

  case class AliasNode(
    nameNode: NameNode,
    typeVariables: Vector[TypeVariableNode],
    tpe: TypeExprNode,
    position: Position,
  ) extends TypeDeclNode

  /**
    * @param extended The names of all traits that the struct extends.
    */
  case class StructNode(
    nameNode: NameNode,
    typeVariables: Vector[TypeVariableNode],
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
    typeVariables: Vector[TypeVariableNode],
    extended: Vector[TypeExprNode],
    position: Position,
  ) extends TypeDeclNode

}
