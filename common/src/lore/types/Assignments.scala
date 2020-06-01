package lore.types

import scala.collection.mutable

// TODO: For later: If we want to support sum types and intersection types, they (kind of) require us to
//       branch with the substitution.
//class AssignmentFamily

// TODO: Rename to something less generic.
class Assignments private () {
  private val assignments = mutable.HashMap[TypeVariable, List[Type]]()
  private var areAssignmentsCompatible = false

  /**
    * Assigns the given type to the type variable.
    */
  def assign(tv: TypeVariable, tpe: Type): Unit = {
    val types = assignments.getOrElse(tv, Nil)
    assignments.put(tv, tpe +: types)
  }

  /**
    * In the given type, replaces all instances of type variables registered here with their assigned representative.
    * You must have checked for assignment compatibility first (by reading isConsistent for the first time).
    */
  def substituteInto(in: Type): Type = {
    if (!areAssignmentsCompatible) {
      throw new RuntimeException("Assignments are incompatible or haven't been checked yet!")
    }

    in match {
      case tv: TypeVariable => assignments.get(tv) match {
        case None => tv
        case Some(types) => types.head
      }
      case ProductType(components) => ProductType(components.map(substituteInto))
      case SumType(types) => SumType(types.map(substituteInto))
      case IntersectionType(types) => IntersectionType(types.map(substituteInto))
      case ListType(element) => ListType(substituteInto(element))
      case MapType(key, value) => MapType(substituteInto(key), substituteInto(value))
      case t => t
    }
  }

  /**
    * Whether these substitutions are consistent. We essentially check two properties:
    *   1. All types assigned to the same variable are compatible (equal) to each other.
    *   2. Type assignments are consistent with type bounds.
    */
  lazy val isConsistent: Boolean = {
    // Check the "compatible assignments" property.
    areAssignmentsCompatible = assignments.forall { case (_, assignments) =>
      assignments.sliding(2).forall {
        case List(left, right) =>
          // Since equality is transitive, we don't have to compare all types to each other.
          left == right
        case List(_) =>
          // List(_).sliding(2) will return List(_), so we have to manually evaluate to true for the special
          // case of one-element lists.
          true
      }
    }

    if (areAssignmentsCompatible) {
      // Check the "type bounds" property.
      assignments.forall { case (variable, representative :: _) =>
        val actualBound = substituteInto(variable.bound)
        Subtyping.isSubtype(representative, actualBound)
      }
    } else false
  }
}

object Assignments {
  /**
    * Creates an Assignments object that represents all type variables in t2 which have been assigned types
    * from t1.
    */
  def of(t1: Type, t2: Type): Assignments = {
    val assignments = new Assignments
    assign(t1, t2)(assignments)
    assignments
  }

  private def assign(t1: Type, t2: Type)(implicit assignments: Assignments): Unit = {
    def unsupportedSubstitution: Nothing = {
      throw new RuntimeException("Intersection and sum type type variable substitutions are not yet supported.")
    }

    (t1, t2) match {
      case (_, tv2: TypeVariable) => assignments.assign(tv2, t1)
      case (d1: DeclaredType, d2: DeclaredType) => ???
      case (l1: ListType, l2: ListType) => assign(l1.element, l2.element)
      case (m1: MapType, m2: MapType) =>
        // TODO: Is this correct?
        assign(m1.key, m2.key)
        assign(m1.value, m2.value)
      case (p1: ProductType, p2: ProductType) =>
        if (p1.components.size == p2.components.size) {
          p1.components.zip(p2.components).foreach { case (c1, c2) => assign(c1, c2) }
        }

      // Substituting types into intersection types and sum types is quite complex, since the substitution mechanism
      // suddenly has more than one option where to substitute into. Take, for example, a sum type A | B, in which we
      // try to substitute a type C. Should A or B become C? Surely not both A and B can be C. And even if we have a
      // structurally similar type C | D, should A = C and B = D or A = D and B = C? There are multiple possibilities.
      // We don't have to assign variables perfectly to check for subtyping, but there are some possibilities which
      // will lead to the answer "not a subtype", while others will lead to the answer "is a subtype". Hence, we have
      // to consider all these possibilities, which makes the algorithm vastly more complex.
      case (_: IntersectionType, _) => unsupportedSubstitution
      case (_, _: IntersectionType) => unsupportedSubstitution
      case (_: SumType, _) => unsupportedSubstitution
      case (_, _: SumType) => unsupportedSubstitution

      // In all other cases, there is no need to substitute anything. Note that component types can't contain
      // type variables, currently, as they expect a class type.
      case _ =>
    }
  }

}
