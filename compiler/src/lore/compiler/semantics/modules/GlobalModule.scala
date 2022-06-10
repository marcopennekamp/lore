package lore.compiler.semantics.modules

import lore.compiler.core.Position
import lore.compiler.semantics.bindings.TermBinding
import lore.compiler.semantics.{BindingKind, NamePath}

/**
  * A global module combines a module's definitions across all source fragments. It also represents modules as a
  * palpable binding in scopes, so that module accesses can be resolved properly.
  */
class GlobalModule(val name: NamePath) extends TermBinding {
  private var modulePositions: Vector[Position] = Vector.empty
  private var types: Map[String, Vector[Position]] = Map.empty
  private var terms: Map[String, Vector[Position]] = Map.empty

  def positions: Vector[Position] = modulePositions
  def typeNames: Set[String] = types.keySet
  def termNames: Set[String] = terms.keySet

  def addModulePosition(position: Position): Unit = {
    modulePositions = modulePositions :+ position
  }

  /**
    * Adds a binding `memberName` to the global module.
    */
  def add(memberName: String, position: Position, bindingKind: BindingKind): Unit = {
    val entries = entriesOf(bindingKind)
    val positions = entries.getOrElse(memberName, Vector.empty)
    val updatedEntries = entries.updated(memberName, positions :+ position)
    bindingKind match {
      case BindingKind.Type => types = updatedEntries
      case BindingKind.Term => terms = updatedEntries
    }
  }

  /**
    * Adds a type and a term `memberName` to the global module, e.g. for struct types and struct constructors/objects.
    */
  def add(memberName: String, position: Position): Unit = {
    add(memberName, position, BindingKind.Type)
    add(memberName, position, BindingKind.Term)
  }

  /**
    * Whether this module has a member `memberName` of kind `bindingKind`.
    */
  def has(memberName: String, bindingKind: BindingKind): Boolean = entriesOf(bindingKind).contains(memberName)

  /**
    * Returns an absolute name path if this module has a member `memberName` of kind `bindingKind`, and `None`
    * otherwise.
    */
  def getAbsolutePath(memberName: String, bindingKind: BindingKind): Option[NamePath] = {
    if (has(memberName, bindingKind)) Some(name + memberName) else None
  }

  /**
    * Get all member positions for the member `memberName` of kind `bindingKind`. If the member doesn't exist, an empty
    * list is returned.
    */
  def getMemberPositions(memberName: String, bindingKind: BindingKind): Vector[Position] = {
    entriesOf(bindingKind).getOrElse(memberName, Vector.empty)
  }

  private def entriesOf(bindingKind: BindingKind): Map[String, Vector[Position]] = bindingKind match {
    case BindingKind.Type => types
    case BindingKind.Term => terms
  }

  override def toString: String = name.toString
}
