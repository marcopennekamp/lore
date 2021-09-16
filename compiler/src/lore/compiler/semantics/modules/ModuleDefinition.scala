package lore.compiler.semantics.modules

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.scopes.Binding

/**
  * ModuleDefinitions are shallow due to the mostly lexical nature of modules. ModuleDefinition is mostly used to
  * represent modules as a palpable binding in scopes, so that module accesses can be resolved properly.
  *
  * @param positions All positions where the module is declared.
  */
class ModuleDefinition(
  val name: NamePath,
  val positions: Vector[Position],
) extends Binding {
  override def toString: String = name.toString
}
