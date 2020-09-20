package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.structures.DeclaredTypeDefinition
import lore.compiler.types.{ComponentType, Subtyping}

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
    *
    * To avoid cyclic reasoning with the subtyping of owned-by types, we have to remove the owned-by rule respective
    * to component types (c1 <= t2 if c2.ownedBy <= t2) from the subtyping rules for this check. Removing it as a sort
    * of blanket ban won't affect the result negatively, because the entity's type already contains all types that
    * are needed to determine ownership. There is no need to look at other owned-by types of the entity's (legal)
    * components.
    *
    * Here is an example of such cyclic reasoning:
    *
    *   struct C1 owned by +A
    *   struct E { component C1 }
    *
    * C1 cannot be a component of E, because A is not a component of E. But with the full subtyping rules, we'd have:
    *
    *   1. C1.ownedBy <= +A  [This is a trivial identity, as C1.ownedBy is +A.]
    *   2. +C1        <= +A  [Because C1's owned-by type is a subtype of +A, we can say that an entity of type +C1 is
    *                         of course a subtype of +A, too.]
    *   3. E          <= +A  [This holds because to check whether an entity is a subtype of a component type, we look
    *                         at all component types (in this case only C1) and determine whether +C1 is a subtype of
    *                         +A. This then defers to the check +C1 <= +A.]
    *
    * In summary E <= +A holds, because we assume that E owns C1 and can thus be treated as +C1. But we want to check
    * whether E can in fact own C1 and cannot use the owned-by type of C1 to prove that.
    */
  private def verifyCanOwn(entity: DeclaredTypeDefinition, componentType: ComponentType): Verification = {
    if (Subtyping.noOwnedBy.isSubtype(entity.tpe, componentType.underlying.ownedBy)) Verification.succeed
    else Compilation.fail(EntityCannotOwnComponent(entity, componentType))
  }

}
