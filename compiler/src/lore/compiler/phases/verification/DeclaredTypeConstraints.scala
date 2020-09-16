package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.structures.DeclaredTypeDefinition
import lore.compiler.types.DeclaredType

object DeclaredTypeConstraints {

  /**
    * Verifies:
    *   1. The owned-by type of a declared type must be a subtype of each owned-by type of its supertypes. If the
    *      declared type or supertype has no owned-by type, we assume Any.
    *   2. All entity constraints hold if the declared type is an entity.
    */
  def verify(definition: DeclaredTypeDefinition): Verification = {
    verifyOwnedBy(definition).flatMap { _ =>
      if (definition.tpe.isEntity) {
        EntityConstraints.verify(definition)
      } else Verification.succeed
    }
  }

  case class OwnedByMustBeSubtype(definition: DeclaredTypeDefinition, supertype: DeclaredType) extends Error(definition) {
    override def message: String = s"The owned-by type ${definition.ownedBy} of declared type ${definition.name} must be a subtype of" +
      s" supertype ${supertype.name}'s owned-by type ${supertype.ownedBy}."
  }

  /**
    * The owned-by type of a declared type must be a subtype of each owned-by type of its supertypes.
    */
  def verifyOwnedBy(definition: DeclaredTypeDefinition): Verification = {
    definition.tpe.supertypes.map {
      case supertype: DeclaredType =>
        if (definition.ownedBy <= supertype.ownedBy) Verification.succeed
        else Compilation.fail(OwnedByMustBeSubtype(definition, supertype))
      case _ => Verification.succeed
    }.simultaneous.verification
  }

}
