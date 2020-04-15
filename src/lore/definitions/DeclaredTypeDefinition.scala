package lore.definitions

import lore.compiler.C
import lore.types.DeclaredType

/**
  * The definition of a declared type.
  */
trait DeclaredTypeDefinition {
  def name: String
  def tpe: DeclaredType
  def supertypeDefinition: Option[DeclaredTypeDefinition]

  /**
    * Verifies all deferred typings introduced via [[TypingDeferred]].
    */
  def verifyDeferredTypings: C[Unit]
}
