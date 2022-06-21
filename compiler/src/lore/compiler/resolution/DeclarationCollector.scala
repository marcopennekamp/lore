package lore.compiler.resolution

import lore.compiler.semantics.NamePath
import lore.compiler.syntax.DeclNode.{FunctionNode, GlobalVariableNode, SpecNode}
import lore.compiler.syntax.{DeclNode, NamedDeclNode, TypeDeclNode}
import lore.compiler.utils.CollectionExtensions.VectorMapExtension

/**
  * [[DeclarationCollector]] collects [[DeclNode]]s by kind for the [[DeclarationResolver]] to use. Nodes that the
  * declaration resolver doesn't need are omitted. The declaration collector guarantees that all declarations are
  * unique, as duplicates are not collected.
  */
class DeclarationCollector {
  var typeDeclarations: Map[NamePath, TypeDeclNode] = Map.empty
  var globalVariableDeclarations: Vector[GlobalVariableNode] = Vector.empty
  var multiFunctionDeclarations: Map[NamePath, Vector[FunctionNode]] = Map.empty
  var specDeclarations: Vector[SpecNode] = Vector.empty

  def collect(node: NamedDeclNode, fullName: NamePath): Unit = {
    node match {
      case node: TypeDeclNode =>
        typeDeclarations += fullName -> node

      case node: GlobalVariableNode =>
        globalVariableDeclarations :+= node

      case node: FunctionNode =>
        multiFunctionDeclarations = multiFunctionDeclarations.appended(fullName, node)

      case _ =>
    }
  }

  def collect(node: SpecNode): Unit = specDeclarations :+= node
}
