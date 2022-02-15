package lore.compiler.poem

import lore.compiler.assembly.PropertyOrder
import lore.compiler.core.CompilationException
import lore.compiler.utils.CollectionExtensions.VectorExtension

/**
  * @param names The names must be ordered lexicographically.
  */
case class PoemMetaShape(names: Vector[String]) {
  if (!names.isSorted) {
    throw CompilationException(s"The property names $names of a shape must be sorted lexicographically.")
  }

  if (names.length != names.distinct.length) {
    throw CompilationException(s"The property names $names of a shape may not contain duplicates.")
  }
}

object PoemMetaShape {
  def build(names: Vector[String]): PoemMetaShape = PoemMetaShape(PropertyOrder.sort(names)(identity))
}
