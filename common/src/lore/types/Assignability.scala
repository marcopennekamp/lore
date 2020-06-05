package lore.types

object Assignability {

  // Why we need to use isAssignable here:
  //    class T1
  //    class T2 extends T1
  //    B <: T1
  //    A <: B
  //    T2 is assignable to A if A.lowerBound (Nothing) < T2 < A.upperBound (B)
  //      using isAssignable: T2 < B <== T2 < B.upperBound and T2 > B.lowerBound <== T2 < T1 and T2 > Nothing
  //                          Nothing < T2 and T2 < T1 are trivial
  //      using isSubtype:    T2 < B <= T2 < B.lowerBound <= T2 < Nothing
  //                          OOPS!
  private val rules: Seq[Subtyping.Rule] = Seq[Subtyping.Rule](
    { case (v1: TypeVariable, v2: TypeVariable) => innerIsAssignable(v2.lowerBound, v1.lowerBound) && innerIsAssignable(v1.upperBound, v2.upperBound) },
    { case (v1: TypeVariable, t2) => innerIsAssignable(v1.upperBound, t2) },
    { case (t1, v2: TypeVariable) => innerIsAssignable(v2.lowerBound, t1) && innerIsAssignable(t1, v2.upperBound) },
  ) ++ Subtyping.monomorphicRules(innerIsAssignable)

  /**
    * Whether t1 is more specific than t2.
    */
  def isMoreSpecific(t1: Type, t2: Type): Boolean = {
    isAssignable(t1, t2) && !isAssignable(t2, t1)
  }

  /**
    * Whether t1 is as specific as t2.
    */
  def isEquallySpecific(t1: Type, t2: Type): Boolean = {
    t1 == t2 || isAssignable(t1, t2) && isAssignable(t2, t1)
  }

  /**
    * Whether t1 fits into t2 as an input. This essentially checks whether ANY instance of t1 could be assignable to
    * ONE instance of t2.
    *
    * To check assignability with parametric types, we also have to ensure type variable consistency.
    */
  def isAssignable(t1: Type, t2: Type): Boolean = {
    // Two types are trivially assignable to each other if they are equal.
    if (t1 == t2) return true

    // If t2 is parametric, we have to check that assignments to its type variables are consistent. This effectively
    // ensures that all instances of the type variable are assigned the SAME type. Only if t2's type variables can
    // be assigned to consistently can we provably assign t1 to t2.
    // This MUST NOT be done when assignability is checked recursively.
    if (t2.isParametric) {
      val allocation: TypeVariableAllocation = TypeVariableAllocation.of(t1, t2)
      if (!allocation.isConsistent) {
        return false
      }
    }

    innerIsAssignable(t1, t2)
  }

  private def innerIsAssignable(t1: Type, t2: Type): Boolean = {
    // t1 is assignable to t2 if any of the rules are true.
    rules.exists(rule => rule.isDefinedAt((t1, t2)) && rule.apply((t1, t2)))
  }

}