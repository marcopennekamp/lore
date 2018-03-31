package lore.types

object Subtyping {

  /**
    * @return Whether t1 is a supertype of t2.
    */
  def isSupertype(t1: Type, t2: Type): Boolean = isSubtype(t2, t1)

  /**
    * @return Whether t1 is a subtype of t2.
    */
  def isSubtype(t1: Type, t2: Type): Boolean = {
    (t1, t2) match {
      case (l1: LabelType, l2: LabelType) => l1 == l2 || isSubtype(l1.supertype, l2)

      // An intersection type i1 is the subtype of an intersection type i2, if all types in i2 are subsumed by i1.
      case (i1: IntersectionType, i2: IntersectionType) => i2.types.forall(ic2 => i1.isComponentTypeSubtypeOf(ic2))
      case (i1: IntersectionType, _) => i1.isComponentTypeSubtypeOf(t2)
      // A non-intersection type t1 is the subtype of an intersection type i2, if t1 is the subtype of all types in i2.
      case (_, i2: IntersectionType) => i2.types.forall(ic2 => isSubtype(t1, ic2))

      // A sum type s1 is the subtype of a sum type s2, if all types in s1 are also (possibly supertyped) in s2.
      case (s1: SumType, s2: SumType) => s1.types.forall(sc1 => s2.types.exists(sc2 => isSubtype(sc1, sc2)))
      // A type t1 is the subtype of a sum type s2, if t1 is a subtype of any of the types in s2.
      case (_, s2: SumType) => s2.types.exists(sc2 => isSubtype(t1, sc2))
      // TODO: Subtyping for (sum type, other type). A rule: A <: C and B <: C => A | B <: C

      // A tuple type tt1 is the subtype of a tuple type tt2, if both types have the same component types and each
      // component type of tt1 is a subtype of the component type in tt2 that is at the same position.
      case (tt1: TupleType, tt2: TupleType) =>
        tt1.components.size == tt2.components.size && tt1.components.zip(tt2.components).forall {
          case (ttc1, ttc2) => isSubtype(ttc1, ttc2)
        }

      // The any type is subtype of none (except itself) and supertype of all types.
      case (_, AnyType) => true
      case (AnyType, _) => false
    }
  }

  /**
    * @return Whether t1 is a strict supertype of t2.
    */
  def isStrictSupertype(t1: Type, t2: Type): Boolean = t1 != t2 && isSupertype(t1, t2)

  /**
    * @return Whether t1 is a strict subtype of t2.
    */
  def isStrictSubtype(t1: Type, t2: Type): Boolean = t1 != t2 && isSubtype(t1, t2)

}
