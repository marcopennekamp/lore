package lore.definitions

import lore.compiler.Compilation.Verification
import lore.types.{DeclaredType, TypingDeferred}

/**
  * The definition of a declared type.
  */
trait DeclaredTypeDefinition extends PositionedDefinition {
  def name: String
  def tpe: DeclaredType
  def supertypeDefinition: Option[DeclaredTypeDefinition]

  /**
    * Verifies all deferred typings introduced via [[TypingDeferred]].
    */
  def verifyDeferredTypings: Verification
}
