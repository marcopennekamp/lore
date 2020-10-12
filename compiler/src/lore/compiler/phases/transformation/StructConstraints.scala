package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.{ComponentDefinition, MemberDefinition, StructDefinition}
import lore.compiler.types.{ComponentType, DeclaredType}
import lore.compiler.utils.CollectionExtensions.VectorExtension

import scala.collection.mutable

object StructConstraints {

  /**
    * Verifies:
    *   1. Members must be unique.
    *   2. Each component in a struct may not share an ownable supertype with any other component defined in the same
    *      struct.
    *   3. For each component declared via an implemented trait, there must be a component defined in the struct
    *      that backs the component.
    */
  def verify(definition: StructDefinition)(implicit registry: Registry): Verification = {
    verifyMembersUnique(definition)
      .flatMap(_ => verifyComponentsNoSharedSupertype(definition))
      .flatMap(_ => verifyComponentsImplemented(definition))
  }

  case class MemberDuplicateDeclaration(definition: StructDefinition, member: MemberDefinition) extends Error(member) {
    override def message = s"The member ${member.name} is declared twice in the struct ${definition.name}."
  }

  /**
    * Verifies that this struct's members are unique.
    */
  private def verifyMembersUnique(definition: StructDefinition): Verification = {
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
    override def message: String = s"The following components are invalid because they share an ownable supertype" +
      s" $supertype: ${components.map(_.name).mkString(", ")}. Components may not share an ownable supertype, because" +
      s" component types such as +C have to remain unambiguous for all possible entities. To resolve this issue, consider" +
      s" declaring $supertype as independent."
  }

  /**
    * Verifies that no two components share the same ownable supertype, which is a restriction outlined in detail in the
    * specification.
    *
    * Algorithm:
    *   1. Map each component to its ownable root supertypes, excluding component types.
    *      - We have to exclude component types because they cannot be nested. A type +(+C) is impossible, so if
    *        two components A and B have a type +C in common, that should not be of concern. We only have to look
    *        at common traits and structs.
    *   2. For each ownable root supertype, create a "bucket" in a hash map where components can be put. If two or
    *      more components share the same bucket, they share that supertype and are thus invalid.
    */
  private def verifyComponentsNoSharedSupertype(definition: StructDefinition): Verification = {
    val buckets = mutable.HashMap[DeclaredType, Vector[ComponentDefinition]]()

    definition.components.foreach { component =>
      component.tpe.ownableRootSupertypes.foreach { supertype =>
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
  private def verifyComponentsImplemented(definition: StructDefinition): Verification = {
    // Since the struct's own components are also part of the struct's supertypes, we have some redundant checks
    // in the current approach, but this is outweighed by its simplicity. Note that while inheritedComponentTypes is a
    // list of types with component types that are subsumed removed, this does not invalidate its usage: If a component
    // type +C is subsumed by a component type +C1, and C1 just so happens to be a component declared in the struct
    // to be checked, we do not have to check +C but only +C1. If an entity is +C1, of course it will also be a +C,
    // and the removal from the list is just another way of ticking the box.
    definition.tpe.inheritedComponentTypes.toVector.map { componentType =>
      if (definition.components.exists(_.tpe <= componentType.underlying)) Verification.succeed
      else Compilation.fail(ComponentNotImplemented(definition, componentType))
    }.simultaneous.verification
  }

}
