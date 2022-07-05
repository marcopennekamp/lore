package lore.compiler.types

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.definitions.TypeDefinition
import lore.compiler.syntax.TypeDeclNode

sealed abstract class BasicType(override val name: NamePath, val kind: Kind) extends TypeDefinition with NamedType {
  override def node: TypeDeclNode = throw CompilationException("Basic types don't have associated syntax nodes.")
  override def isInitialized: Boolean = true
  override def position: Position = Position.internal
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
    /**
      * The maximum run-time integer value supported by the VM (61-bit signed integer).
      */
    val maximum: Long = 1152921504606846975L

    /**
      * The minimum run-time integer value supported by the VM (61-bit signed integer).
      */
    val minimum: Long = -1152921504606846976L
  }

  case object Real extends BasicType(NamePath("Real"), Kind.Real)

  case object Boolean extends BasicType(NamePath("Boolean"), Kind.Boolean)

  case object String extends BasicType(NamePath("String"), Kind.String)
}
