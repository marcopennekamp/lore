package lore.compiler.semantics.modules

import lore.compiler.core.CompilationException
import lore.compiler.semantics.NameKind
import lore.compiler.syntax.DeclNode.ModuleNode
import lore.compiler.syntax.Node.NamePathNode
import lore.compiler.syntax.{BindingDeclNode, DeclNode, TypeDeclNode}

/**
  * An IndexedModule is a global entry in the [[GlobalModuleIndex]].
  *
  * TODO (modules): Rename to GlobalModule?
  */
class IndexedModule {
  var typeNames: Set[String] = Set.empty
  var bindingNames: Set[String] = Set.empty

  def add(node: DeclNode): Unit = {
    node match {
      case ModuleNode(NamePathNode(Vector(nameNode)), _, _, _) => addBindingName(nameNode.value)
      case node: ModuleNode => throw CompilationException(
        s"Module nodes must be denested when being added to the IndexedModule. Position: ${node.position}."
      )
      case node: BindingDeclNode => addBindingName(node.simpleName)
      case node: TypeDeclNode => addTypeName(node.simpleName)
    }
  }

  private def addTypeName(name: String): Unit = {
    typeNames = typeNames + name
  }

  private def addBindingName(name: String): Unit = {
    bindingNames = bindingNames + name
  }

  /**
    * Whether this module has a member named `name` given `nameKind`.
    */
  def has(name: String, nameKind: NameKind): Boolean = namesOf(nameKind).contains(name)

  private def namesOf(nameKind: NameKind): Set[String] = nameKind match {
    case NameKind.Type => typeNames
    case NameKind.Binding => bindingNames
  }
}
