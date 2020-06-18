package lore.types

import lore.types.TypeRelations.Rule

object Assignability {

  private val rules: List[Rule] = List[Rule](
    // A type variable v1 is assignable to a type variable v2 if all the types described by v1 are also described by v2,
    // because that means that no matter what type we assign to v1, v2 will also be able to accommodate this type.
    // The subsequent two cases must be equal to this case if we set the type variable's upper bound to the given
    // monomorphic type and its lower bound to Nothing.
    { case (v1: TypeVariable, v2: TypeVariable) => innerIsAssignable(v2.lowerBound, v1.lowerBound) && innerIsAssignable(v1.upperBound, v2.upperBound) },
    // A type variable v1 is assignable to a type t2 if all the types described by v1 are assignable to t2. No matter
    // which type we assign to v1, t2 will always be able to accommodate such a type.
    { case (v1: TypeVariable, t2) if t2.isMonomorphic => innerIsAssignable(v1.upperBound, t2) },
    // A type t1 is assignable to a type variable v2 if the types described by v2 contain t1. This means that t1
    // has to adhere to the lower and upper bounds of v2.
    // TODO: We have a slight problem: When a class type is invariant, we don't want C[String] to be assignable to
    //       C[Any], UNLESS we really have a C[X <: Any]. Maybe to check types against type variables,
    //       we have to move away from the notion of subtyping and to a concept of "instance equality", which
    //       calculates equality on the basis of whether one type can be equal to the other type if all type
    //       variables are instanced correctly.
    { case (t1, v2: TypeVariable) if t1.isMonomorphic => innerIsAssignable(v2.lowerBound, t1) && innerIsAssignable(t1, v2.upperBound) },
  ) ++ TypeRelations.monomorphicSubtypingRules(innerIsAssignable, innerIsEquallySpecific)

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
    if (t2.isPolymorphic) {
      val allocation: TypeVariableAllocation = TypeVariableAllocation.of(t1, t2)
      if (!allocation.isConsistent) {
        return false
      }
    }

    innerIsAssignable(t1, t2)
  }

  /**
    * The isAssignable function without type variable consistency checks, which can only be supported at the
    * top level of assignability checking.
    */
  private def innerIsAssignable(t1: Type, t2: Type): Boolean = {
    t1 == t2 || TypeRelations.inRelation(rules)(t1, t2)
  }

  /**
    * The isEquallySpecific function without type variable consistency checks, which can only be supported at the
    * top level of assignability checking.
    */
  private def innerIsEquallySpecific(t1: Type, t2: Type): Boolean = {
    t1 == t2 || innerIsAssignable(t1, t2) && innerIsAssignable(t2, t1)
  }

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

}
