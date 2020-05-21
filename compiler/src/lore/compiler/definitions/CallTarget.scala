package lore.compiler.definitions

import lore.types.Type

trait CallTarget {
  def name: String
  def outputType: Type
}
