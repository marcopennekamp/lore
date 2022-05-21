package lore.compiler.semantics.modules

import lore.compiler.core.Position
import lore.compiler.semantics.scopes.Binding
import lore.compiler.semantics.{NameKind, NamePath}

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
