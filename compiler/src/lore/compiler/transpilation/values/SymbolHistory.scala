package lore.compiler.transpilation.values

import lore.compiler.target.Target
import lore.compiler.transpilation.RuntimeNames

import scala.collection.immutable.HashSet

/**
  * Memorizes all symbols that are transpiled so that they can be properly interned. All symbols of the same name must
  * point to the same type and value target variables.
  */
class SymbolHistory {

  var symbols: Set[String] = HashSet.empty

  def targetType(name: String): Target.Variable = {
    include(name)
    RuntimeNames.symbolType(name)
  }

  def targetValue(name: String): Target.Variable = {
    include(name)
    RuntimeNames.symbolValue(name)
  }

  private def include(name: String): Unit = {
    symbols = symbols + name
  }

}
