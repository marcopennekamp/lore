package lore.compiler.semantics.members

import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.VectorExtension

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
    *   - Intersection Type: In the case of an intersection type, if a member is part of one of the types, it is also
    *     a member of the intersection type. This leads to a possible problem with ambiguities: if two or more parts
    *     define a member of the same name, the definition is technically ambiguous. We can resolve these as such:
    *       - Case 1: All members (bearing the same name) are assignable and their types are exactly equal. The
    *                 resulting member is also assignable.
    *       - Case 2: At least one member is assignable or mutable. Bearing in mind case 1 does not apply, the
    *                 resulting member is nonassignable and mutable.
    *       - Case 3: All members are nonassignable and immutable. Then the resulting member is also nonassignable
    *                 and immutable.
    *       - The type of the resulting member is always the intersection of all member types, because we are saying
    *         "this property has types A, B, C, ... at the same time." For example, if we have a type
    *         `{ x: Real } & { x: String }`, `x` is both a `Real` and a `String`. Because `{ x: Real }` and
    *         `{ x: String }` must both be supertypes of `{ x: Real } & { x: String }`, `{ x: Real & String }` is the
    *         only sensible solution. See also [[ShapeType.combine]].
    */
  def members(tpe: Type): MemberMap = {
    tpe match {
      case structType: StructType =>
        // Note that we cannot implement this via .asShapeType, since we need the members to be initialized with
        // mutability. The shape view loses mutability.
        HashMap(structType.definition.properties.map(property => property.name -> property.asMember): _*)

      case traitType: TraitType => members(traitType.asShapeType)

      case shapeType: ShapeType =>
        shapeType.properties.map { case (name, property) => name -> property.asMember }

      case tv: TypeVariable => tv.upperBound.members

      case IntersectionType(types) =>
        val allMembers = types.toVector.map(_.members).flatMap(_.values)
        val combinedMembers = allMembers.groupBy(_.name).map {
          // No ambiguity, so we don't have to construct a common member.
          case (_, Vector(member)) => member

          // Ambiguity, so we have to resolve this according to the cases outlined in the comment above.
          case (name, members) =>
            // Case 1.
            if (members.forall(_.isAssignable) && members.allEqual(_.tpe)) {
              members.head
            } else {
              val tpe = IntersectionType.construct(members.map(_.tpe))

              // Case 2.
              if (members.exists(m => m.isAssignable || m.isMutable)) {
                Member(name, tpe, isMutable = true)
              } else {
                // Case 3.
                Member(name, tpe)
              }
            }
        }.toVector
        HashMap(combinedMembers.map(m => (m.name, m)): _*)

      case _ => HashMap.empty: MemberMap
    }
  }

}
