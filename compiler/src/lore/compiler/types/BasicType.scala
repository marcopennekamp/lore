package lore.compiler.types

import lore.compiler.semantics.NamePath

sealed abstract class BasicType(override val name: NamePath) extends NamedType {
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

  case object Real extends BasicType(NamePath("Real"))

  case object Int extends BasicType(NamePath("Int")) {
    /**
      * The maximum safe run-time integer value supported by Javascript.
      */
    val maxSafeInteger: Long = 9007199254740991L

    /**
      * The minimum safe run-time integer value supported by Javascript.
      */
    val minSafeInteger: Long = -9007199254740991L
  }

  case object Boolean extends BasicType(NamePath("Boolean"))

  case object String extends BasicType(NamePath("String"))
}
