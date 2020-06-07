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
          left == right
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
        val actualLowerBound = Substitution.substitute(assignments, variable.lowerBound)
        val actualUpperBound = Substitution.substitute(assignments, variable.upperBound)
        // Example 1:
        //    function foo(a: A, b: B) where A, B <: A
        //    foo(5, 5.5)
        //    A = Int, B = Real
        //    (A) Check Nothing <= Int <= Any
        //    (B) Check Nothing <= Real <= Int
        //    Example doesn't pertain to the current situation.
        // Example 2:
        //    function foo(a: A, b: B) where A, B <: A
        //    function bar(c: C) where C = foo(c, 5)
        //    All types that C can represent must be a subset of the types that A can represent. Otherwise there
        //    is one type with which C can be instanced that isn't applicable to A, hence we can't call foo with
        //    a first argument of type C.
        //    foo(c, 5)
        //    A = C, B = Int
        //    (A) Check Nothing <= C <= Any
        //       using subtyping: Nothing <= C.lowerBound is true, C.upperBound <= Any is true
        //       using assignability:
        //        Nothing <= C: C.lowerBound <= Nothing is true, Nothing <= C.upperBound is true
        //        C <= Any: C.upperBound <= Any is true
        //    (B) Check Nothing <= Int <= C
        //       using subtyping: Nothing <= Int is true, Int <= C.lowerBound is false
        //       using assignability:
        //        Nothing <= Int is true
        //        C.lowerBound <= Int and Int <= C.upperBound is true
        //          --> Leads to the false conclusion because we cannot assign C to A and then also assign Int to B,
        //              because B is supposed to be a subtype of A, while Int isn't necessarily such a subtype.
        // Example 3:
        //    class X
        //    class Y extends X
        //    class Z extends Y
        //    class W extends Z
        //    function foo(a: A, b: B) where A >: Z <: X, B <: A
        //    function bar(c: C) where C = foo(c, Z())
        //    foo(c, Z())
        //    A = C, B = Z
        //    (A) Check Z <= C <= X
        //       using subtyping: Z <= C.lowerBound is false, C.upperBound <= X is false
        //                    --> C's lower bound must be at most Z, C's upper bound must be at least X
        //                        This is the structurally correct conclusion.
        //       using assignability:
        //        Z <= C: C.lowerBound <= Z is true, Z <= C.upperBound is true
        //        C <= X: C.upperBound <= X is false
        //          If we added an upper bound to C, this would suddenly be valid. But of course it can't be, as
        //          the lower bound of A doesn't agree yet.
        // Example 4:
        //    function foo(a: A, b: B) where A >: Z <: X, B <: A
        //    function bar(c: C, d: D) where C, D <: C = foo(c, d)
        //    foo(c, d)
        //    A = C, B = D
        //    (A) Check ...
        // In conclusion:
        //    When we want to check whether type variable assignments adhere to type bounds, we have to check
        //    whether ALL instances of the assigned type fit within the bounds of the type variable. Hence we
        //    check whether ALL instances of the lower bound are subtypes of ALL instances of the assigned type
        //    (if they are not, we can choose a type t1 permitted by the assigned type which is lower than a
        //    type t2 permitted by the lower bound) and whether ALL instances of the upper bound are supertypes of
        //    ALL instances of the assigned type (conversely, if they are not, we can choose a type t1 permitted
        //    by the assigned type which is higher than a type t2 permitted by the upper bound).
        //    In the style of a proof of contradiction:
        //        - Assume that we want to assign a type t1 to a type variable v2. To be assignable to v2, t1 has
        //        to fit within v2's upper and lower bounds. More concretely, as type variables are already instanced
        //        (in this context of consistency checking), t1 has to fit between instanced upper and lower bounds u2
        //        and l2. These instanced bounds may be monomorphic or polymorphic themselves.
        //        - Lemma 1: To check the lower bound, we have to prove that all instances of l2 are subtypes of all
        //        instances of t1.
        //          - Proof: Assume the statement in the lemma is not the case. That means that we can find instances
        //          a of t1 and b of l2 for which a < b. That is, we find an instance in l2 which is not a subtype of
        //          an instance of t1. But then we can't assign t1 to v2, because if we instantiate t1 as a, we can
        //          choose an instance of l2 such as b for which t1 doesn't adhere to the lower bound of v2.
        //          - Example: From example 3 above, assume we have Z <: A <: X and W <: C <: X with A = C.
        //          t1 is a type variable C in this case, with lower bound W and upper bound X. We can prove that C
        //          can't be assigned to A by choosing a = W and b = Z. If C is W, we cannot assign C to A, because
        //          A cannot accommodate a type W. If you look at example 3, the function call foo(c, Z()) has to
        //          be valid for ALL instances of C. Multiple dispatch doesn't allow calling foo with a value of type
        //          W.
        //          TODO: This shows a certain problem, actually. How do we ensure that foo can be called with a run-time
        //                value of type W? If we, say, call foo with type Z, but then later put a W into that variable at
        //                run-time, multiple dispatch would fail. Can we even allow lower bounds in parameter positions?
        //                Or can we only allow lower bounds for output types and only upper bounds for input types? Or
        //                is this all check-able, comparable to the totality constraint?
        //                The easiest fix is probably disabling lower bounds in parameter positions. A harder fix would
        //                be to actually enforce that input types of multi-functions eventually "meet" at tuples of
        //                Nothing.
        //                We can also get rid of the compile-time requirement that we cannot have empty-fit errors in
        //                any circumstance at run-time. That is, we would ignore lower bounds and deal with it at
        //                run-time. Maybe we can throw a warning at compile-time. If checking the totality constraint
        //                for polymorphic types becomes impossible or tedious, we can also choose to do our best at
        //                compile-time and then still allow the occasional empty-fit error to happen at run-time.
        //        - Lemma 2: To check the upper bound, we have to prove that all instances of u2 are supertypes of all
        //        instances of t1.
        //          - Proof: Assume the statement in the lemma is not the case. That means that we can find instances
        //          a of t1 and b of u2 for which b < a. That is, we find an instance in u2 which is not a supertype
        //          of an instance of t1. But then we can't assign t1 to v2, because if we instantiate t1 as a, we can
        //          choose an instance of u2 such as b for which t1 doesn't adhere to the upper bound of v2.
        //        - Combining lemma 1 and 2, we arrive at the definite conclusion that this style of bounds checking
        //        requires using polymorphic subtyping in the way it is specified in the line of code below.
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
