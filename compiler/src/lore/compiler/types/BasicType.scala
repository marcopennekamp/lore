package lore.compiler.types

import lore.compiler.semantics.NamePath

sealed abstract class BasicType(override val name: NamePath, val kind: Kind) extends NamedType {
  /**
    * Whether the basic type describes numeric values (Int, Real).
    */
  def isNumeric: Boolean = false

  override val hashCode: Int = name.hashCode
}

object BasicType {
  /**
    * The "top" type which is the supertype of all possible types.
    */
  case object Any extends BasicType(NamePath("Any"), Kind.Any)

  /**
    * The "bottom" type which is the subtype of all types.
    */
  case object Nothing extends BasicType(NamePath("Nothing"), Kind.Nothing)

  case object Int extends BasicType(NamePath("Int"), Kind.Int) {
    override def isNumeric: Boolean = true

    /**
      * The maximum run-time integer value supported by the VM (61-bit signed integer).
      */
    val maximum: Long = 1152921504606846975L

    /**
      * The minimum run-time integer value supported by the VM (61-bit signed integer).
      */
    val minimum: Long = -1152921504606846976L
  }

  case object Real extends BasicType(NamePath("Real"), Kind.Real) {
    override def isNumeric: Boolean = true
  }

  case object Boolean extends BasicType(NamePath("Boolean"), Kind.Boolean)

  case object String extends BasicType(NamePath("String"), Kind.String)
}
