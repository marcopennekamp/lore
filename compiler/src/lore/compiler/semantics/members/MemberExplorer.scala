package lore.compiler.semantics.members

import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.{MapVectorExtension, VectorExtension}

import scala.collection.immutable.HashMap

/**
  * For any given type, returns a list of members that can be accessed through the type. This is trivial for
  * struct types and shapes, but quickly becomes interesting when intersection types are considered.
  */
object MemberExplorer {

  /**
    * Returns all members associated with this type.
    *
    * Given a type, we have the following way to determine all members of that type:
    *
    *   - Declared Type/Shape: The members of a struct or shape are simply their declared properties. The members of
    *     a trait are the properties of the trait viewed as a shape.
    *   - Type Variable: A type variable's members are defined by its upper bound. If a member is a member of the
    *     upper bound type, it must also be present in all possible instances of the type variable. If it wasn't, we
    *     would be violating a basic contract of polymorphism.
    *   - Sum Type: If all parts of a sum type have a member `name`, the sum type also has this member. The combined
    *     member's type is a sum type of all member types.
    *   - Intersection Type: If one part of an intersection type has a member `name`, the intersection type also has
    *     this member. The combined member's type is an intersection of all member types.
    *
    * The situation for sum and intersection types leads to a possible problem with ambiguities. If two or more parts
    * induce a sum/intersection type member, its definition is technically ambiguous. We can resolve that as such:
    *   - Case 1: All members (bearing the same name) are assignable and their types are exactly equal. The resulting
    *             member is also assignable.
    *   - Case 2: At least one member is assignable or mutable. Bearing in mind case (1) does not apply, the resulting
    *             member is nonassignable and mutable.
    *   - Case 3: All members are nonassignable and immutable. Then the resulting member is also nonassignable and
    *             immutable.
    */
  def members(tpe: Type): MemberMap = {
    tpe match {
      case structType: StructType =>
        // Note that we cannot implement this via .asShapeType, since we need the members to be initialized with
        // mutability. The shape view loses mutability.
        HashMap(structType.properties.map(instance => instance.property.name -> instance.asMember): _*)

      case traitType: TraitType => members(traitType.asShapeType)

      case shapeType: ShapeType =>
        shapeType.properties.map { case (name, property) => name -> property.asMember }

      case tv: TypeVariable => tv.upperBound.members

      case SumType(parts) =>
        val partsVector = parts.toVector
        val commonMembers = partsVector.map(_.members)
          .merged
          // A member is a common member if it occurs in all member maps. This means that the merged member map must
          // contain exactly as many members as there are parts in the sum type.
          .filter { case (_, members) => members.length == partsVector.length }
        combineMembers(commonMembers, SumType.construct(_))

      case IntersectionType(types) =>
        val allMembers = types.toVector.map(_.members).flatMap(_.values)
        combineMembers(allMembers.groupBy(_.name), IntersectionType.construct(_))

      case _ => HashMap.empty: MemberMap
    }
  }

  private def combineMembers(members: Map[String, Vector[Member]], combineMemberTypes: Vector[Type] => Type): MemberMap = {
    members.map {
      // No ambiguity, so we don't have to construct a common member.
      case (name, Vector(member)) => name -> member

      // Ambiguity, so we have to resolve this according to the cases outlined above.
      case (name, members) =>
        // Case 1.
        if (members.forall(_.isAssignable) && members.allEqual(_.tpe)) {
          name -> members.head
        } else {
          val tpe = combineMemberTypes(members.map(_.tpe))

          // Case 2.
          if (members.exists(m => m.isAssignable || m.isMutable)) {
            name -> Member(name, tpe, isMutable = true)
          } else {
            // Case 3.
            name -> Member(name, tpe)
          }
        }
    }
  }

}
