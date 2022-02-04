package lore.compiler.assembly

object PropertyOrder {

  /**
    * The VM requires shape and struct properties to be ordered by their names lexicographically. We have to sort
    * properties to build construction instructions, poem types, and poem values.
    */
  def sort[A](properties: Vector[A])(getName: A => String): Vector[A] = properties.sortBy(getName)

}
