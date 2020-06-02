package lore.types

object Assignability {

  private val rules: Seq[Subtyping.Rule] = Seq[Subtyping.Rule](
    // TODO: We have a slight problem: When a class type is invariant, we don't want C[String] to be a subtype of
    //       C[Any], UNLESS we really have a C[String] <: C[X <: Any]. Maybe to check types against type variables,
    //       we have to move away from the notion of subtyping and to a concept of "instance equality", which
    //       calculates equality on the basis of whether one type can be equal to the other type if all type
    //       variables are instanced correctly.
    // TODO: Another problem: What we do here is not strictly subtyping but rather bounds checking. Subtyping would
    //       actually mean that we have to check whether a type t1 is ALWAYS a subtype of v2. This means ensuring
    //       that the lower bound v2.lowerBound is a supertype of t1, so as to ensure that t1 can always be assigned
    //       to v2. This means that subtyping for multiple dispatch is different from the general notion of
    //       subtyping: We want to check whether the concrete input type COULD BE a subtype of the parameter tuple,
    //       not whether it actually IS.
    //       So, to check subtyping against a type variable, we have to do "assignability" bounds checking, and to
    //       check whether a given input type is more specific than another input type (for multiple dispatch), we
    //       have to "mock" assignability with Assignments and then check whether the assigned type fits into the
    //       bounds. There is a special case when we have type variables on both sides. Take (A) and (B) for example.
    //       If we substitute A into B, we have to check that A's upper bound is a subtype of B's lower bound and
    //       A's lower bound is a supertype of B's lower bound. That is, the types that A describes fit into the
    //       range of types described by B.
    // TODO: This also means that we need to replace the multiple dispatch notion of subtyping with a notion
    //       of specificity, which is equal to subtyping for monomorphic types, but defined with more refinement
    //       for polymorphic types.
    { case (v1: TypeVariable, v2: TypeVariable) => isAssignable(v1.upperBound, v2.upperBound) },
    { case (v1: TypeVariable, t2) => isAssignable(v1.upperBound, t2) },
    { case (t1, v2: TypeVariable) => isAssignable(t1, v2.upperBound) },
  ) ++ Subtyping.monomorphicRules(isAssignable)

  // TODO: Alternative name: isSpecialization.
  /**
    * Whether t1 is more specific than t2.
    */
  def isMoreSpecific(t1: Type, t2: Type): Boolean = {
    isAssignable(t1, t2) && !isAssignable(t2, t1)
  }

  def isEquallySpecific(t1: Type, t2: Type): Boolean = {
    isAssignable(t1, t2) && isAssignable(t2, t1)
  }

  /**
    * Whether t1 fits into t2 as an input. This essentially checks whether a value of t1 could be assignable to
    * a variable with the type t2.
    */
  def isAssignable(t1: Type, t2: Type): Boolean = {
    ???
  }

}
