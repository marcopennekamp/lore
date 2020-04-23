package lore.definitions

import lore.compiler.Compilation.Verification
import lore.compiler.Registry
import lore.types.{DeclaredType, TypingDeferred}

/**
  * The definition of a declared type.
  */
trait DeclaredTypeDefinition extends PositionedDefinition {
  def name: String
  def tpe: DeclaredType
  def supertypeDefinition: Option[DeclaredTypeDefinition]

  /**
    * Verifies all constraints for a given declared type.
    */
  def verifyConstraints(implicit registry: Registry): Verification
}
