package lore.compiler.functions

import lore.compiler.types.Type

trait CallTarget {
  def name: String
  def outputType: Type
}
