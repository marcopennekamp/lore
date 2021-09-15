package lore.compiler.semantics.modules

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.scopes.Binding

/**
  * ModuleDefinitions are shallow due to the mostly lexical nature of modules. ModuleDefinition is mostly used to
  * represent modules as a palpable binding in scopes, so that module accesses can be resolved properly.
  */
case class ModuleDefinition(name: NamePath) extends Binding {
  override def toString: String = name.toString
}
