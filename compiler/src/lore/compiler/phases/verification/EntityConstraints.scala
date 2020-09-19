package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.structures.DeclaredTypeDefinition
import lore.compiler.types.ComponentType

object EntityConstraints {

  /**
    * Verifies:
    *   1. If a component has an owned-by type, we verify that the component can be in fact owned by this entity.
    */
  def verify(entity: DeclaredTypeDefinition): Verification = {
    entity.tpe.inheritedComponentTypes.map(t => verifyCanOwn(entity, t)).toVector.simultaneous.verification
  }

  case class EntityCannotOwnComponent(entity: DeclaredTypeDefinition, componentType: ComponentType) extends Error(entity) {
    override def message: String = s"The entity ${entity.name} cannot own the component ${componentType.underlying.name} due" +
      s" to the component's owned-by restriction."
  }

  /**
    * Verifies that the given entity can in fact own a component of the given type. (In principle, with the information
    * available at compile-time.)
    */
  private def verifyCanOwn(entity: DeclaredTypeDefinition, componentType: ComponentType): Verification = {
    if (entity.tpe <= componentType.underlying.ownedBy) Verification.succeed
    else Compilation.fail(EntityCannotOwnComponent(entity, componentType))
  }

}
