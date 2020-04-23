package lore.compiler.phases.verification

import lore.compiler.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.{Compilation, Registry}
import lore.definitions.ClassDefinition
import lore.types.{AnyType, Subtyping, Type}

object ClassConstraints {
  case class ClassMayNotExtendEntity(definition: ClassDefinition) extends Error(definition) {
    override def message = s"The class ${definition.name} extends an entity but is not an entity itself."
  }

  case class OwnedByMustBeSubtype(definition: ClassDefinition, ownedBy: Type, superOwnedBy: Type) extends Error(definition) {
    override def message = s"The owned-by type $ownedBy of class ${definition.name} must be a subtype of the superclass's owned-by type $superOwnedBy."
  }

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
    (
      verifyEntityInheritance(definition),
      verifyOwnedBy(definition),
    ).simultaneous.verification
  }

  /**
    * Verifies that the given class does not extend an entity if it is itself not an entity.
    */
  def verifyEntityInheritance(definition: ClassDefinition): Verification = {
    // If the definition is not an entity, its supertype may not be an entity.
    if (!definition.isEntity && definition.supertypeDefinition.exists(_.isEntity)) {
      Compilation.fail(ClassMayNotExtendEntity(definition))
    } else Verification.succeed
  }

  /**
    * Verifies that the owned-by type of the given class is a subtype of the owned-by type of the superclass.
    */
  def verifyOwnedBy(definition: ClassDefinition): Verification = {
    // We have to assume Any, because this handles a special case where the owned-by declaration of a class has
    // been forgotten, despite the superclass having its own owned-by type.
    val ownedBy = definition.tpe.ownedBy.map(_.tpe).getOrElse(AnyType)

    // Here, we assume Any in case the supertype is None or its owned-by type is None. In both cases, the omission
    // of such a declaration means that the supertype's owned-by type is effectively Any.
    val superOwnedBy = definition.tpe.supertype.flatMap(_.ownedBy).map(_.tpe).getOrElse(AnyType)

    // Now we just have to check whether the owned-by type is actually a subtype.
    if (!Subtyping.isSubtype(ownedBy, superOwnedBy)) {
      Compilation.fail(OwnedByMustBeSubtype(definition, ownedBy, superOwnedBy))
    } else Verification.succeed
  }
}
