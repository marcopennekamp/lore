package lore.types

import scala.collection.mutable

/**
  * Allows checking whether type variable assignments are consistent.
  *
  * An assignment is a single binding. An allocation is a nested set of assignments.
  */
class TypeVariableAllocation() {
  private val allocation = mutable.HashMap[TypeVariable, List[Type]]()

  /**
    * Adds the given type as an assignment of the type variable.
    */
  def addAssignment(tv: TypeVariable, tpe: Type): Unit = {
    val types = allocation.getOrElse(tv, Nil)
    allocation.put(tv, tpe +: types)
  }

  /**
    * Whether these allocations are consistent. We essentially check two properties:
    *   1. All types assigned to the same variable are compatible (equal) to each other.
    *   2. Assigned types are consistent with their variable's type bounds.
    */
  lazy val isConsistent: Boolean = {
    // Check the "compatible assignments" property.
    allocation.forall { case (_, possibleAssignments) =>
      possibleAssignments.sliding(2).forall {
        case List(left, right) =>
          // Since equality is transitive, we don't have to compare all types to each other.
          left == right // TODO: Test equality using assignability or polymorphic subtyping?
        case List(_) =>
          // List(_).sliding(2) will return List(_), so we have to manually evaluate to true for the special
          // case of one-element lists.
          true
      }
    } && {
      // Check the "type bounds" property.
      val assignments = allocation.view.mapValues { case representative :: _ => representative }.toMap
      assignments.forall { case (variable, tpe) =>
        // TODO: We might have to substitute multiple times until no substitutions occur. We should verify this
        //       using a fitting example.
        //       - I don't think this is the case, because we don't substitute allocated variables into the bounds,
        //         as all variables should be instanced with types not containing the allocated variables. At most
        //         we will have type variables from the type we are assigning type from.
        val actualLowerBound = Substitution.substitute(assignments, variable.lowerBound)
        val actualUpperBound = Substitution.substitute(assignments, variable.upperBound)
        Subtyping.isSubtype(actualLowerBound, tpe) && Subtyping.isSubtype(tpe, actualUpperBound)
      }
    }
  }
}

object TypeVariableAllocation {
  /**
    * Creates a type variable allocation that represents all type variables in t2 which have been assigned types
    * from t1.
    */
  def of(t1: Type, t2: Type): TypeVariableAllocation = {
    val allocation = new TypeVariableAllocation
    assign(t1, t2)(allocation)
    allocation
  }

  private def assign(t1: Type, t2: Type)(implicit allocation: TypeVariableAllocation): Unit = {
    def unsupportedSubstitution: Nothing = {
      throw new RuntimeException("Intersection and sum type type variable allocations are not yet supported.")
    }

    (t1, t2) match {
      case (_, tv2: TypeVariable) => allocation.addAssignment(tv2, t1)
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

      // Allocating types to intersection types and sum types is quite complex, since the allocation mechanism
      // suddenly come upon more than one possible allocation. Take, for example, a sum type A | B, to which we
      // try to assign a type C. Should A or B become C? Surely not both A and B can be C (unless the sum type
      // is trivial). And even if we have a structurally similar type C | D, should A = C and B = D or A = D and
      // B = C? There are multiple possibilities.
      case (_: IntersectionType, _) => unsupportedSubstitution
      case (_, _: IntersectionType) => unsupportedSubstitution
      case (_: SumType, _) => unsupportedSubstitution
      case (_, _: SumType) => unsupportedSubstitution

      // In all other cases, there is no need to assign anything. Note that component types can't contain
      // type variables, currently, as they expect a class type.
      // TODO: Component types will be able to contain type variables when classes become polymorphic.
      case _ =>
    }
  }

}
