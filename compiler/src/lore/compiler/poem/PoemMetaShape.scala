package lore.compiler.poem

import lore.compiler.assembly.PropertyOrder

/**
  * @param names The names must be ordered lexicographically.
  */
case class PoemMetaShape(names: Vector[String])

object PoemMetaShape {
  def build(names: Vector[String]): PoemMetaShape = PoemMetaShape(PropertyOrder.sort(names, identity))
}
