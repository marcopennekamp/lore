package lore.compiler.semantics.modules

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.semantics.{NameKind, NamePath}
import lore.compiler.syntax.DeclNode.ModuleNode
import lore.compiler.syntax.Node.NamePathNode
import lore.compiler.syntax.{BindingDeclNode, DeclNode, TypeDeclNode}

/**
  * An IndexedModule is a global entry in the [[GlobalModuleIndex]].
  *
  * TODO (modules): Rename to GlobalModule?
  */
class IndexedModule(val name: NamePath) {
  var typeNames: Set[String] = Set.empty
  var bindingNames: Set[String] = Set.empty
  var positions: Vector[Position] = Vector.empty

  def add(node: DeclNode): Unit = {
    node match {
      case ModuleNode(NamePathNode(Vector(nameNode)), _, _, _) => addBindingName(nameNode.value)
      case node: ModuleNode => throw CompilationException(
        s"Module nodes must be denested when being added to the IndexedModule. Module name: ${node.namePathNode}. Position: ${node.position}."
      )
      case node: BindingDeclNode => addBindingName(node.simpleName)
      case node: TypeDeclNode => addTypeName(node.simpleName)
    }
  }

  def add(name: String, kind: NameKind): Unit = kind match {
    case NameKind.Type => addTypeName(name)
    case NameKind.Binding => addBindingName(name)
  }

  private def addTypeName(name: String): Unit = {
    typeNames = typeNames + name
  }

  private def addBindingName(name: String): Unit = {
    bindingNames = bindingNames + name
  }

  def add(position: Position): Unit = {
    positions = positions :+ position
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