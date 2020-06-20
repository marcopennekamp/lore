package lore.types

import lore.types.TypeRelations.Rule

// TODO: Rename to PolymorphicSubtyping.

trait Subtyping {

  // TODO: Do we rather need to define type equality in terms of subtyping (t1 <= t2 && t2 <= t1)? I suspect
  //       new edge cases especially with the introduction of polymorphic types. Of course, this might severely
  //       affect performance and thus needs to be looked at first.

  def polymorphicRules: List[Rule] = List[Rule](
    // Two variables are subtypes of each other if v1's upper bound agrees with v2's lower bound. This is easy to
    // see: We want all instances of v1 to be subtypes of all instances of v2. The least specific instance of v1
    // is its upper bound. The most specific instance of v2 is its lower bound. Hence we require these to meet.
    { case (v1: TypeVariable, v2: TypeVariable) => isSubtype(v1.upperBound, v2.lowerBound) },
    // All instances of v1 are definitely subtypes of t2 if v1's upper bound is a subtype of t2, hence ensuring
    // that any instance of v1 has t2 as a supertype.
    { case (v1: TypeVariable, t2) if t2.isMonomorphic => isSubtype(v1.upperBound, t2) },
    // t1 is definitely a subtype of all instances of v2 if v2's lower bound ensures that instances of v2 are always
    // a supertype of t1.
    { case (t1, v2: TypeVariable) if t1.isMonomorphic => isSubtype(t1, v2.lowerBound) },
  ) ++ TypeRelations.monomorphicSubtypingRules(isSubtype, _ == _)

  /**
    * Whether t1 is a subtype of t2.
    */
  def isSubtype(t1: Type, t2: Type): Boolean = {
    // TODO: Do we need type variable consistency checking for proper subtyping?
    //       We need it for assignability, of course. Specificity was the motivation to add it in the first place.
    //       But subtyping is a different kind of beast. We will have to first implement assignability, replace all
    //       relevant usages of subtyping with assignability, and then list where we are actually using subtyping.
    //       Then we can come up with usage examples and decide whether we need consistency checking.
    //       Example:
    //        Consider a class C[A](x: A, y: A)
    //        What is the type of C(5, 'x')?
    //        Since we can LUB any two types, this would trivially be C[Any].
    //        In this example, we don't need to check type variable consistency, because we can INFER the value
    //        of A at compile-time. There is no need to guarantee that all arguments agree in their type, because
    //        we aren't deciding anything at run-time.
    t1 == t2 || TypeRelations.inRelation(polymorphicRules)(t1, t2)
  }

  /**
    * Whether t1 is a supertype of t2.
    */
  def isSupertype(t1: Type, t2: Type): Boolean = isSubtype(t2, t1)

  /**
    * Whether t1 is a strict subtype of t2.
    */
  def isStrictSubtype(t1: Type, t2: Type): Boolean = t1 != t2 && isSubtype(t1, t2)

  /**
    * Whether t1 is a strict supertype of t2.
    */
  def isStrictSupertype(t1: Type, t2: Type): Boolean = t1 != t2 && isSupertype(t1, t2)
}

object Subtyping extends Subtyping
