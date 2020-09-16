package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.{ComponentDefinition, MemberDefinition, StructDefinition}
import lore.compiler.types.{ComponentType, DeclaredType}
import lore.compiler.utils.CollectionExtensions.FilterTypeVectorExtension

import scala.collection.mutable

object StructConstraints {

  /**
    * Verifies:
    *   1. Members must be unique.
    *   2. All declared type constraints hold.
    *   3. Each component in a struct may not share a supertype with any other component defined in the same struct.
    *   4. TODO: For each component declared via an implemented trait, there must be a defined component in the struct
    *            that backs that component.
    */
  def verify(definition: StructDefinition)(implicit registry: Registry): Verification = {
    for {
      // We verify in order instead of simultaneously because, for example, a supertype sharing error may actually
      // be a non-uniqueness error and so on.
      _ <- verifyMembersUnique(definition)
      _ <- DeclaredTypeConstraints.verify(definition)
      _ <- verifyComponentsNoSharedSupertype(definition)
      _ <- verifyComponentsImplemented(definition)
    } yield ()
  }

  case class MemberDuplicateDeclaration(definition: StructDefinition, member: MemberDefinition) extends Error(member) {
    override def message = s"The member ${member.name} is declared twice in the struct ${definition.name}."
  }

  /**
    * Verifies that this struct's members are unique.
    */
  def verifyMembersUnique(definition: StructDefinition): Verification = {
    definition.members.groupBy(_.name).values.map {
      case Vector(_) => Verification.succeed
      case members if members.size > 1 => Compilation.fail(MemberDuplicateDeclaration(definition, members.head))
    }.toVector.simultaneous.verification
  }

  case class ComponentsShareSupertype(
    definition: StructDefinition,
    supertype: DeclaredType,
    components: Vector[ComponentDefinition],
  ) extends Error(definition) {
    override def message: String = s"The following components are invalid because they share a supertype" +
      s" $supertype: ${components.map(_.name).mkString(", ")}. Components may not share a supertype, because" +
      s" component types such as +C have to remain unambiguous for all possible entities."
  }

  /**
    * Verifies that no two components share the same supertype, which is a restriction outlined in detail in the
    * specification.
    *
    * Algorithm:
    *   1. Map each component to its root supertypes, excluding component types.
    *      - We have to exclude component types because they cannot be nested. A type +(+C) is impossible, so if
    *        two components A and B have a type +C in common, that should not be of concern. We only have to look
    *        at common traits and structs.
    *   2. For each root supertype, create a "bucket" in a hash map where components can be put. If two or more
    *      components share the same bucket, they share that supertype and are thus invalid.
    */
  def verifyComponentsNoSharedSupertype(definition: StructDefinition): Verification = {
    val buckets = mutable.HashMap[DeclaredType, Vector[ComponentDefinition]]()

    definition.components.foreach { component =>
      component.tpe.rootSupertypes.filterType[DeclaredType].foreach { supertype =>
        buckets.updateWith(supertype) {
          case None => Some(Vector(component))
          case Some(bucket) => Some(bucket :+ component)
        }
      }
    }

    buckets.view.toVector.map {
      case (_, Vector(_)) => Verification.succeed
      case (supertype, bucket) => Compilation.fail(ComponentsShareSupertype(definition, supertype, bucket))
    }.simultaneous.verification
  }

  case class ComponentNotImplemented(definition: StructDefinition, componentType: ComponentType) extends Error(definition) {
    override def message: String = s"The struct ${definition.name} should implement component type $componentType but doesn't."
  }

  /**
    * Verifies that each component type ascribed to the struct via any trait is actually implemented, i.e. that
    * defined components are backed by an actual component defined in the struct.
    */
  def verifyComponentsImplemented(definition: StructDefinition): Verification = {
    // Since the struct's own components are also part of the struct's supertypes, we have some redundant checks
    // in the current approach, but this is outweighed by its simplicity. Note that while componentTypes is a list
    // of types with component types that are subsumed removed, this does not invalidate its usage: If a component
    // type +C is subsumed by a component type +C1, and C1 just so happens to be a component declared in the struct
    // to be checked, we do not have to check +C but only +C1. If an entity is +C1, of course it will also be a +C,
    // and the removal from the list is just another way of ticking the box.
    definition.tpe.componentTypes.map { componentType =>
      if (definition.components.exists(_.tpe <= componentType)) Verification.succeed
      else Compilation.fail(ComponentNotImplemented(definition, componentType))
    }.simultaneous.verification
  }

}
