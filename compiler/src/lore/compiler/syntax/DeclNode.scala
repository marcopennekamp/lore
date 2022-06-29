package lore.compiler.syntax

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.syntax.DeclNode.TypeVariableNode
import lore.compiler.syntax.Node.{NameNode, NamePathNode, NamedNode}
import lore.compiler.types.TypeVariable.Variance

/**
  * All top-level declaration nodes.
  */
sealed trait DeclNode extends Node {
  /**
    * The [[LocalModule]] this DeclNode is declared in. Each DeclNode has exactly one associated local module.
    */
  var localModule: LocalModule = _
}

sealed trait NamedDeclNode extends DeclNode {
  /**
    * The simple name of the declared entity.
    */
  def simpleName: String

  /**
    * The full name of the DeclNode. This is only available once [[localModule]] has been set.
    *
    * TODO (multi-import): Maybe we can get this equivalent name path from the [[lore.compiler.semantics.modules.BindingModuleMember]].
    * TODO (multi-import): This probably shouldn't even exist or be a `def`.
    */
  lazy val fullName: NamePath = localModule.globalModule.name + simpleName
}

sealed trait SimpleNamedDeclNode extends NamedDeclNode {
  def nameNode: NameNode
  override def simpleName: String = nameNode.value
}

/**
  * Top-level term declarations.
  */
sealed trait TermDeclNode extends NamedDeclNode

/**
  * Top-level type declarations.
  */
sealed trait TypeDeclNode extends NamedDeclNode {
  def typeVariables: Vector[TypeVariableNode]
}

object DeclNode {

  case class ModuleNode(
    namePathNode: NamePathNode,
    atRoot: Boolean,
    imports: Vector[ImportNode],
    members: Vector[DeclNode],
    position: Position,
  ) extends TermDeclNode with NamedDeclNode {
    def namePath: NamePath = namePathNode.namePath

    /**
      * The simple name of a module declaration is the <b>first</b> segment of the module's full name path. This is the
      * name by which the module will be referred to in scopes.
      */
    override def simpleName: String = namePath.headName

    // The definition of "full name" for a module node is a little iffy. It could either be the name up to the head
    // segment, or the name that includes all of `namePath`. `fullName` for modules isn't actually used, so disallowing
    // its use is fine.
    override lazy val fullName: NamePath = throw CompilationException("Module nodes do not define full names.")
  }

  case class ImportNode(
    namePathNode: NamePathNode,
    isWildcard: Boolean,
    position: Position,
  ) extends Node

  case class GlobalVariableNode(
    nameNode: NameNode,
    tpe: TypeExprNode,
    value: ExprNode,
    position: Position,
  ) extends TermDeclNode with SimpleNamedDeclNode

  /**
    * Function declarations. These include action declarations, which are resolved as syntactic sugar by the parser.
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
  ) extends TermDeclNode with SimpleNamedDeclNode {
    def isAbstract: Boolean = body.isEmpty
  }

  object FunctionNode {
    def from(
      nameNode: NameNode,
      typeVariables: Vector[TypeVariableNode],
      parameters: Vector[ParameterNode],
      outputType: Option[TypeExprNode],
      body: Option[ExprNode],
      position: Position,
    ): FunctionNode = {
      DeclNode.FunctionNode(nameNode, parameters, outputType.getOrElse(TypeExprNode.UnitNode(position)), typeVariables, body, position)
    }
  }

  case class ParameterNode(
    nameNode: Option[NameNode],
    tpe: TypeExprNode,
    position: Position,
  ) extends Node {
    def name: Option[String] = nameNode.map(_.value)
  }

  case class SpecNode(
    description: String,
    isTest: Boolean,
    isBenchmark: Boolean,
    body: ExprNode,
    position: Position,
  ) extends DeclNode

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

  case class AliasNode(
    nameNode: NameNode,
    typeVariables: Vector[TypeVariableNode],
    tpe: TypeExprNode,
    isStructAlias: Boolean,
    position: Position,
  ) extends TypeDeclNode with SimpleNamedDeclNode

  /**
    * @param extended The names of all traits that the struct extends.
    */
  case class StructNode(
    nameNode: NameNode,
    isObject: Boolean,
    typeVariables: Vector[TypeVariableNode],
    extended: Vector[TypeExprNode],
    properties: Vector[PropertyNode],
    position: Position,
  ) extends TypeDeclNode with SimpleNamedDeclNode

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
  ) extends TypeDeclNode with SimpleNamedDeclNode

}
