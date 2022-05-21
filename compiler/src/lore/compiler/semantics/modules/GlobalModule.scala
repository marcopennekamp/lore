package lore.compiler.semantics.modules

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.core.Position
import lore.compiler.semantics.scopes.Binding
import lore.compiler.semantics.{NameKind, NamePath}
import lore.compiler.syntax.DeclNode.{ModuleNode, SpecNode}
import lore.compiler.syntax.Node.NamePathNode
import lore.compiler.syntax.{BindingDeclNode, DeclNode, TypeDeclNode}

/**
  * A global module combines a module's definitions across all source fragments. It also represents modules as a
  * palpable binding in scopes, so that module accesses can be resolved properly.
  */
class GlobalModule(val name: NamePath) extends Binding {
  private var modulePositions: Vector[Position] = Vector.empty
  private var types: Map[String, Vector[Position]] = Map.empty
  private var bindings: Map[String, Vector[Position]] = Map.empty

  def positions: Vector[Position] = modulePositions
  def typeNames: Set[String] = types.keySet
  def bindingNames: Set[String] = bindings.keySet

  def addModulePosition(position: Position): Unit = {
    modulePositions = modulePositions :+ position
  }

  def add(node: DeclNode): Unit = {
    node match {
      case ModuleNode(NamePathNode(Vector(nameNode)), _, _, position) => add(nameNode.value, position, NameKind.Binding)
      case node: ModuleNode => throw CompilationException(
        s"Module nodes must be denested when being added to the GlobalModule. Module name: ${node.namePathNode}. Position: ${node.position}."
      )
      case node: DeclNode.StructNode => add(node.simpleName, node.position)
      case node: DeclNode.AliasNode if node.isStructAlias => add(node.simpleName, node.position)
      case node: BindingDeclNode => add(node.simpleName, node.position, NameKind.Binding)
      case node: TypeDeclNode => add(node.simpleName, node.position, NameKind.Type)
      case _: SpecNode =>
        // Specs do not need to be added to the global module, because they can't be referenced from Lore code.
    }
  }

  /**
    * Adds a type or binding `memberName` to the global module.
    */
  def add(memberName: String, position: Position, nameKind: NameKind): Unit = {
    val entries = entriesOf(nameKind)
    val positions = entries.getOrElse(memberName, Vector.empty)
    val updatedEntries = entries.updated(memberName, positions :+ position)
    nameKind match {
      case NameKind.Type => types = updatedEntries
      case NameKind.Binding => bindings = updatedEntries
    }
  }

  /**
    * Adds a type and a binding `memberName` to the global module, e.g. for struct types and struct
    * constructors/objects.
    */
  def add(memberName: String, position: Position): Unit = {
    add(memberName, position, NameKind.Type)
    add(memberName, position, NameKind.Binding)
  }

  /**
    * Whether this module has a member `memberName` of kind `nameKind`.
    */
  def has(memberName: String, nameKind: NameKind): Boolean = entriesOf(nameKind).contains(memberName)

  /**
    * Returns an absolute name path if this module has a member `memberName` of kind `nameKind`, and `None` otherwise.
    */
  def getAbsolutePath(memberName: String, nameKind: NameKind): Option[NamePath] = {
    if (has(memberName, nameKind)) Some(name + memberName) else None
  }

  /**
    * Get all member positions for the member `memberName`. If the member doesn't exist, the empty list is returned.
    */
  def getMemberPositions(memberName: String, nameKind: NameKind): Vector[Position] = {
    entriesOf(nameKind).getOrElse(memberName, Vector.empty)
  }

  private def entriesOf(nameKind: NameKind): Map[String, Vector[Position]] = nameKind match {
    case NameKind.Type => types
    case NameKind.Binding => bindings
  }

  override def toString: String = name.toString
}
