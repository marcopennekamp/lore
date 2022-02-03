package lore.compiler.types

import lore.compiler.semantics.NamePath

sealed abstract class BasicType(override val name: NamePath) extends NamedType {
  /**
    * Whether the basic type describes primitive values (numbers, strings, booleans).
    */
  def isPrimitive: Boolean = false

  override val hashCode: Int = name.hashCode
}

object BasicType {
  /**
    * The "top" type which is the supertype of all possible types.
    */
  case object Any extends BasicType(NamePath("Any"))

  /**
    * The "bottom" type which is the subtype of all types.
    */
  case object Nothing extends BasicType(NamePath("Nothing"))

  case object Int extends BasicType(NamePath("Int")) {
    override def isPrimitive: Boolean = true

    // TODO (assembly): The safety limits need to be changed for the new VM.

    /**
      * The maximum safe run-time integer value supported by Javascript.
      */
    val maxSafeInteger: Long = 9007199254740991L

    /**
      * The minimum safe run-time integer value supported by Javascript.
      */
    val minSafeInteger: Long = -9007199254740991L
  }

  case object Real extends BasicType(NamePath("Real")) {
    override def isPrimitive: Boolean = true
  }

  case object Boolean extends BasicType(NamePath("Boolean")) {
    override def isPrimitive: Boolean = true
  }

  case object String extends BasicType(NamePath("String")) {
    override def isPrimitive: Boolean = true
  }
}
