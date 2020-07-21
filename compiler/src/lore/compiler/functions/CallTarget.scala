package lore.compiler.functions

import lore.compiler.types.Type

/**
  * The target of any kind of call. This is attached to nodes during verification so that calls are soundly typed
  * and can be properly resolved during transpilation.
  */
trait CallTarget {
  def name: String
  def outputType: Type
}
