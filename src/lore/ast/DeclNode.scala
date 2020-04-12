package lore.ast

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
    name: String, parameters: List[ParameterNode], outputType: TypeExprNode, body: Option[ExprNode],
  ) extends DeclNode {
    def isAbstract: Boolean = body.isEmpty
  }

  case class ParameterNode(name: String, tpe: TypeExprNode)
}

/**
  * Top-level type declarations.
  */
sealed trait TypeDeclNode extends DeclNode
object TypeDeclNode {
  // TODO: We need to add type aliases to the spec... or remove them from the implementation.
  case class AliasNode(name: String, tpe: TypeExprNode) extends TypeDeclNode

  case class LabelNode(name: String, supertypeName: Option[String]) extends TypeDeclNode

  /**
    * Either a class or an entity depending on whether members contains a component.
    */
  case class ClassNode(
    name: String, supertypeName: Option[String], ownedBy: Option[TypeExprNode], isAbstract: Boolean,
    members: List[MemberNode], constructors: List[ConstructorNode],
  ) extends TypeDeclNode

  sealed trait MemberNode
  case class PropertyNode(name: String, tpe: TypeExprNode, isMutable: Boolean) extends MemberNode
  case class ComponentNode(name: String, overrides: Option[String]) extends MemberNode

  /**
    * This node is the default constructor if its name equals the name of the class it belongs to.
    */
  case class ConstructorNode(
    name: String, parameters: List[DeclNode.ParameterNode],
    body: List[StmtNode], continuation: ContinuationNode
  )

  /**
    * The continuation of the construction is deferred to some other constructor or the internal
    * construction mechanism.
    */
  sealed trait ContinuationNode
  case class ConstructorCallNode(name: Option[String], arguments: List[ExprNode]) extends ContinuationNode
  case class ConstructNode(arguments: List[ExprNode], withSuper: Option[ConstructorCallNode]) extends ContinuationNode
}
