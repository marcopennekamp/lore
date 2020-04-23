package lore.compiler.phases.verification

import lore.compiler.Compilation.Verification
import lore.compiler.{Compilation, Registry}
import lore.definitions.ClassDefinition

object ClassConstraints {
  /**
    * Verifies:
    *   1. Non-entity classes may not **extend** entities.
    *   2. The owned-by type of a class must be a subtype of the owned-by type of its superclass. If the class
    *      or superclass has no owned-by type, assume Any.
    *   3. If a component member has an ownedBy type, we verify that the component can be in fact owned by this entity.
    *   4. Each component **overriding** another component must be a subtype of the overridden component.
    *   5. Each constructor must end with a **continuation** node and may not have such a node in any other place.
    */
  def verify(definition: ClassDefinition)(implicit registry: Registry): Verification = {
    // TODO: Implement.
    Verification.succeed
  }
}
