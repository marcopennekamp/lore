package lore.compiler.types

sealed abstract class BasicType(override val name: String) extends NamedType {
  override val hashCode: Int = name.hashCode
}

object BasicType {
  /**
    * The "top" type which is the supertype of all possible types.
    */
  case object Any extends BasicType("Any")

  /**
    * The "bottom" type which is the subtype of all types.
    */
  case object Nothing extends BasicType("Nothing")
  // TODO: How does Nothing interact with abstract subtypes, totality constraint, etc.?

  case object Real extends BasicType("Real")

  case object Int extends BasicType("Int") {
    /**
      * The maximum safe run-time integer value supported by Javascript.
      */
    val maxSafeInteger: Long = 9007199254740991L

    /**
      * The minimum safe run-time integer value supported by Javascript.
      */
    val minSafeInteger: Long = -9007199254740991L
  }

  case object Boolean extends BasicType("Boolean")

  case object String extends BasicType("String")
}
