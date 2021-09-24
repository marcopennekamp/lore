package lore.compiler.semantics.modules

import lore.compiler.core.Position
import lore.compiler.semantics.{NameKind, NamePath}
import lore.compiler.semantics.scopes.Binding

/**
  * ModuleDefinitions are shallow due to the mostly lexical nature of modules. ModuleDefinition is mostly used to
  * represent modules as a palpable binding in scopes, so that module accesses can be resolved properly.
  */
class ModuleDefinition(
  val name: NamePath,
  val globalModule: GlobalModule,
) extends Binding {
  def has(memberName: String, nameKind: NameKind): Boolean = globalModule.has(memberName, nameKind)

  /**
    * All positions at which the module is declared.
    */
  val positions: Vector[Position] = globalModule.modulePositions

  override def toString: String = name.toString
}
