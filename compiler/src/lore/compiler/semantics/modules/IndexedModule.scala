package lore.compiler.semantics.modules

import lore.compiler.core.CompilationException
import lore.compiler.syntax.DeclNode.ModuleNode
import lore.compiler.syntax.Node.{NamePathNode, NamedNode}
import lore.compiler.syntax.{DeclNode, TypeDeclNode}

/**
  * An IndexedModule is a global entry in the [[GlobalModuleIndex]].
  *
  * TODO (modules): Rename to GlobalModule?
  */
class IndexedModule {
  var nodes: Vector[DeclNode] = Vector.empty
  var typeNames: Set[String] = Set.empty
  var bindingNames: Set[String] = Set.empty

  def add(node: DeclNode): Unit = {
    nodes = nodes :+ node

    // TODO (modules): Turn module/variable/function nodes into BindingDeclNodes.
    node match {
      case ModuleNode(NamePathNode(Vector(nameNode)), _, _, _) => addBindingName(nameNode.value)
      case node: ModuleNode => throw CompilationException(
        s"Module nodes must be denested when being added to the IndexedModule. Position: ${node.position}."
      )
      case node: BindingDeclNode => addBindingName(node.name)
      case node: TypeDeclNode => addTypeName(node.name)
    }
  }

  private def addTypeName(name: String): Unit = {
    typeNames = typeNames + name
  }

  private def addBindingName(name: String): Unit = {
    bindingNames = bindingNames + name
  }
}
