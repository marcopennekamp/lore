package lore.types

import lore.types.TypeRelations.Rule

trait Subtyping {

  // TODO: We have a slight problem: When a class type is invariant, we don't want C[String] to be a subtype of
  //       C[Any], UNLESS we really have a C[String] <: C[X <: Any]. Maybe to check types against type variables,
  //       we have to move away from the notion of subtyping and to a concept of "instance equality", which
  //       calculates equality on the basis of whether one type can be equal to the other type if all type
  //       variables are instanced correctly.
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
  ) ++ TypeRelations.monomorphicSubtypingRules(isSubtype)

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
    TypeRelations.inRelation(polymorphicRules)(t1, t2)
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
