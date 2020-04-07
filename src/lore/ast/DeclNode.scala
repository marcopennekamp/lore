package lore.ast

/**
  * All top-level declaration nodes.
  */
sealed trait DeclNode
object DeclNode {
  /**
    * Function declarations. These include action declarations, which are resolved as "syntactic sugar" by the parser.
    */
  case class FunctionNode(
    name: String, parameters: List[ParameterNode], isAbstract: Boolean,
    outputType: TypeExprNode, body: Expr,
  ) extends DeclNode
  case class ParameterNode(name: String, tpe: TypeExprNode)

  /**
    * Top-level type declarations.
    */
  sealed trait TypeDeclNode extends DeclNode
  object TypeDeclNode {
    // TODO: We need to add type aliases to the spec... or remove them from the implementation.
    case class AliasNode(name: String, tpe: TypeExprNode) extends TypeDeclNode

    case class LabelNode(name: String, supertypeName: Option[String]) extends TypeDeclNode

    case class ClassNode(
      name: String, supertypeName: Option[String], isAbstract: Boolean,
      properties: List[PropertyNode], constructors: List[ConstructorNode],
    ) extends TypeDeclNode

    case class EntityNode(
      name: String, supertypeName: Option[String], isAbstract: Boolean,
      members: List[MemberNode], constructors: List[ConstructorNode],
    ) extends TypeDeclNode

    sealed trait MemberNode
    case class PropertyNode(name: String, tpe: TypeExprNode, isMutable: Boolean) extends MemberNode
    case class ComponentNode(name: String, nominalType: TypeExprNode.NominalNode) extends MemberNode

    // TODO: Implement constructor AST.
    case class ConstructorNode()
  }
}
