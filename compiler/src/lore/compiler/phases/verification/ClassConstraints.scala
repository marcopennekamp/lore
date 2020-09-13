package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, CompilationException, Error, Position}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.{StructDefinition, ComponentDefinition, MemberDefinition}
import lore.compiler.types.{BasicType, StructType, Type}

object ClassConstraints {
  /**
    * Verifies:
    *   1. Non-entity classes may not extend entities.
    *   2. The owned-by type of a class must be a subtype of the owned-by type of its superclass. If the class
    *      or superclass has no owned-by type, assume Any.
    *   3. Class properties and components must be unique.
    *   4. If a component member has an ownedBy type, we verify that the component can be in fact owned by this entity.
    *   5. Each component in an entity may not share a superclass with any other component defined in the same entity. [deferred]
    *   6. Each component overriding another component must be a subtype of the overridden component. The overridden
    *      component also has to even exist.
    *   7. All constructor constraints.
    */
  def verify(definition: StructDefinition)(implicit registry: Registry): Verification = {
    val componentVerifications = (
      verifyMembersUnique(definition),
      definition.localComponents.map(verifyOverrides(definition, _)).simultaneous,
      definition.localComponents.map(verifyCanOwn(definition, _)).simultaneous,
    ).simultaneous.flatMap { _ =>
      // Shared superclasses are only verified once all other component-specific errors have been reported.
      // This is simply necessary because otherwise half of all other errors (such as components not being unique)
      // ALSO lead to this error, which is just confusing for the user.
      verifyComponentsDontShareSuperclass(definition)
    }

    (
      verifyEntityInheritance(definition),
      verifyOwnedBy(definition),
      componentVerifications,
      ConstructorConstraints.verify(definition),
    ).simultaneous.verification
  }

  case class ClassMayNotExtendEntity(definition: StructDefinition) extends Error(definition) {
    override def message = s"The class ${definition.name} extends an entity but is not an entity itself."
  }

  /**
    * Verifies that the given class does not extend an entity if it is itself not an entity.
    */
  def verifyEntityInheritance(definition: StructDefinition): Verification = {
    // If the definition is not an entity, its supertype may not be an entity.
    if (!definition.isEntity && definition.supertypeDefinition.exists(_.isEntity)) {
      Compilation.fail(ClassMayNotExtendEntity(definition))
    } else Verification.succeed
  }

  case class OwnedByMustBeSubtype(definition: StructDefinition, ownedBy: Type, superOwnedBy: Type) extends Error(definition) {
    override def message = s"The owned-by type $ownedBy of class ${definition.name} must be a subtype of the superclass's owned-by type $superOwnedBy."
  }

  /**
    * Verifies that the owned-by type of the given class is a subtype of the owned-by type of the superclass.
    */
  def verifyOwnedBy(definition: StructDefinition): Verification = {
    // We have to assume Any, because this handles a special case where the owned-by declaration of a class has
    // been forgotten by the programmer, despite the superclass having its own owned-by type.
    val ownedBy = definition.tpe.ownedBy.getOrElse(BasicType.Any)

    // Here, we assume Any in case the supertype is None or its owned-by type is None. In both cases, the omission
    // of such a declaration means that the supertype's owned-by type is effectively Any.
    val superOwnedBy = definition.tpe.supertypes.flatMap(_.ownedBy).getOrElse(BasicType.Any)

    // Now we just have to check whether the owned-by type is actually a subtype.
    if (!(ownedBy <= superOwnedBy)) {
      Compilation.fail(OwnedByMustBeSubtype(definition, ownedBy, superOwnedBy))
    } else Verification.succeed
  }

  case class MemberAlreadyExistsInSuperclass(definition: StructDefinition, member: MemberDefinition) extends Error(member) {
    override def message = s"The member ${member.name} is already declared in a superclass of class ${definition.name}."
  }

  case class MemberDuplicateDeclaration(definition: StructDefinition, member: MemberDefinition) extends Error(member) {
    override def message = s"The member ${member.name} is already declared twice in the class ${definition.name}."
  }

  /**
    * Verifies that this class's local members are unique and haven't already been declared in a superclass
    * or twice in the given class.
    */
  def verifyMembersUnique(definition: StructDefinition): Verification = {
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

  case class ClassCannotOwnComponent(definition: StructDefinition, component: ComponentDefinition) extends Error(component) {
    override def message = s"The class ${definition.name} cannot own the component ${component.name} due to the component's owned-by restriction."
  }

  /**
    * Verifies that the given class can in fact own the given component. (In principle, with the information
    * available at compile-time.)
    */
  def verifyCanOwn(definition: StructDefinition, component: ComponentDefinition): Verification = {
    val ownershipType = component.tpe.ownedBy.getOrElse(BasicType.Any)
    if (!(definition.tpe <= ownershipType)) {
      Compilation.fail(ClassCannotOwnComponent(definition, component))
    } else Verification.succeed
  }

  case class ComponentsShareSuperclass(definition: StructDefinition, superclass: StructType, components: List[ComponentDefinition]) extends Error(definition) {
    override def message: String = s"The following components illegally share a superclass $superclass: ${components.map(_.name).mkString(", ")}." +
      s" Components may not share a superclass, because component types such as +C have to stay unambiguous for all possible entities."
  }

  /**
    * Verifies that no two components share the same superclass, which is a restriction outlined in detail in the
    * specification.
    */
  def verifyComponentsDontShareSuperclass(definition: StructDefinition): Verification = {
    // Algorithm:
    //   1. Map each component to its highest superclass type.
    //   2. Group components by their superclass type.
    //   3. If there is more than one component in a superclass bucket, they share that superclass.
    definition.components.map(c => (c.tpe.rootSupertype, c))
      .groupBy(_._1)
      .map {
        case (_, List(_)) => Verification.succeed
        case (superclass, bucket) if bucket.size > 1 => Compilation.fail(ComponentsShareSuperclass(definition, superclass, bucket.map(_._2)))
      }
      .toList.simultaneous.verification
  }

  case class OverriddenComponentDoesNotExist(definition: StructDefinition, component: ComponentDefinition) extends Error(component) {
    val overriddenName: String = component.overrides.getOrElse(throw CompilationException("component.overrides should exist."))
    override def message: String = s"The component ${component.name} is supposed to override the component $overriddenName, " +
      s"but $overriddenName is not a supertype component or has already been overridden."
  }

  case class ComponentMustSubtypeOverriddenComponent(definition: StructDefinition, component: ComponentDefinition) extends Error(component) {
    val overriddenName: String = component.overrides.getOrElse(throw CompilationException("component.overrides should exist."))
    override def message: String = s"The component ${component.name} is trying to override the component $overriddenName, " +
      s"but ${component.name} is not a subtype of $overriddenName."
  }

  /**
    * Verifies that a component C2 overriding a component C1 is a subtype of C1. Also ensures that C1 even exists.
    */
  def verifyOverrides(definition: StructDefinition, component: ComponentDefinition)(implicit registry: Registry): Verification = {
    component.overrides.map { overriddenName =>
      // Verify that the overridden component even exists.
      val superComponentNames = definition.supertypeDefinition.map(_.components.map(_.name)).getOrElse(List.empty)
      val exists = if (!superComponentNames.contains(overriddenName)) {
        Compilation.fail(OverriddenComponentDoesNotExist(definition, component))
      } else Verification.succeed

      // Verify that the overriding component is a subtype of the overridden component.
      implicit val position: Position = component.position
      val subtypes = registry.resolveType(overriddenName).flatMap { overriddenType =>
        if (!(component.tpe <= overriddenType)) {
          Compilation.fail(ComponentMustSubtypeOverriddenComponent(definition, component))
        } else Verification.succeed
      }

      (exists, subtypes).simultaneous
    }.toCompiledOption.verification
  }
}
