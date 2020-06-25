package lore.compiler.functions

import lore.types.Type

trait CallTarget {
  def name: String
  def outputType: Type
}
