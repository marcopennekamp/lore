package lore.compiler.phases.verification

import lore.compiler.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.{Compilation, Registry}
import lore.definitions.{ClassDefinition, ComponentDefinition, MemberDefinition}
import lore.types.{AnyType, Subtyping, Type}

object ClassConstraints {
  case class ClassMayNotExtendEntity(definition: ClassDefinition) extends Error(definition) {
    override def message = s"The class ${definition.name} extends an entity but is not an entity itself."
  }

  case class OwnedByMustBeSubtype(definition: ClassDefinition, ownedBy: Type, superOwnedBy: Type) extends Error(definition) {
    override def message = s"The owned-by type $ownedBy of class ${definition.name} must be a subtype of the superclass's owned-by type $superOwnedBy."
  }

  case class ClassCannotOwnComponent(definition: ClassDefinition, component: ComponentDefinition) extends Error(component) {
    override def message = s"The class ${definition.name} cannot own the component ${component.name} due to the component's owned-by restriction."
  }

  case class MemberAlreadyExistsInSuperclass(definition: ClassDefinition, member: MemberDefinition[Type]) extends Error(member) {
    override def message = s"The member ${member.name} is already declared in a superclass of class ${definition.name}."
  }

  case class MemberDuplicateDeclaration(definition: ClassDefinition, member: MemberDefinition[Type]) extends Error(member) {
    override def message = s"The member ${member.name} is already declared twice in the class ${definition.name}."
  }

  /**
    * Verifies:
    *   1. Non-entity classes may not extend entities.
    *   2. The owned-by type of a class must be a subtype of the owned-by type of its superclass. If the class
    *      or superclass has no owned-by type, assume Any.
    *   3. Class properties and components must be unique.
    *   4. If a component member has an ownedBy type, we verify that the component can be in fact owned by this entity.
    *   5. Each component overriding another component must be a subtype of the overridden component.
    *   6. Each constructor must end with a continuation node and may not have such a node in any other place.
    */
  def verify(definition: ClassDefinition)(implicit registry: Registry): Verification = {
    (
      verifyEntityInheritance(definition),
      verifyOwnedBy(definition),
      verifyMembersUnique(definition),
      definition.localComponents.map(verifyCanOwn(definition, _)).simultaneous,
    ).simultaneous.verification
  }

  /**
    * Verifies that the given class does not extend an entity if it is itself not an entity.
    */
  def verifyEntityInheritance(definition: ClassDefinition): Verification = {
    println(definition.name + " " + definition.isEntity + " " + definition.supertypeDefinition.exists(_.isEntity))
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

  /**
    * Verifies that this class's local members are unique and haven't already been declared in a superclass
    * or twice in the given class.
    */
  def verifyMembersUnique(definition: ClassDefinition): Verification = {
    val superMembers = definition.supertypeDefinition.map(_.members).getOrElse(List.empty)
    val superMemberNames = superMembers.map(_.name)
    definition.localMembers.map { member =>
      (

        if (superMemberNames.contains(member.name)) {
          Compilation.fail(MemberAlreadyExistsInSuperclass(definition, member))
        } else Verification.succeed,

        if (definition.localMembers.filterNot(_ == member).map(_.name).contains(member.name)) {
          Compilation.fail(MemberDuplicateDeclaration(definition, member))
        } else Verification.succeed,
        ).simultaneous
    }.simultaneous.verification
  }

  /**
    * Verifies that the given class can in fact own the given component. (In principle, with the information
    * available at compile-time.)
    */
  def verifyCanOwn(definition: ClassDefinition, component: ComponentDefinition): Verification = {
    val ownershipType = component.tpe.ownedBy.map(_.tpe).getOrElse(AnyType)
    if (!Subtyping.isSubtype(definition.tpe, ownershipType)) {
      Compilation.fail(ClassCannotOwnComponent(definition, component))
    } else Verification.succeed
  }
}
