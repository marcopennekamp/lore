package lore.compiler.types

import lore.compiler.types.TypeRelations.Rule

object Subtyping {

  // TODO: Do we rather need to define type equality in terms of subtyping (t1 <= t2 && t2 <= t1)? I suspect
  //       new edge cases especially with the introduction of polymorphic types. Of course, this might severely
  //       affect performance and thus needs to be looked at first.

  def rules: List[Rule] = {
    List(
      // All instances of v1 are definitely subtypes of t2 if v1's upper bound is a subtype of t2, hence ensuring
      // that any instance of v1 has t2 as a supertype.
      { case (v1: TypeVariable, t2) => isSubtype(v1.upperBound, t2) },
      // t1 is definitely a subtype of all instances of v2 if v2's lower bound ensures that instances of v2 are always
      // a supertype of t1.
      { case (t1, v2: TypeVariable) => isSubtype(t1, v2.lowerBound) },

      // A declared type d1 is a subtype of d2 if d1 and d2 are equal or any of d1's hierarchical supertypes equal d2.
      // TODO: Once we have introduced covariant (and possibly contravariant) classes, we will additionally have to
      //       check whether d1's typeArguments are a subtype of d2's type arguments.
      // TODO: Once we introduce parametric declared types, we might have to move this rule out of here.
      { case (d1: DeclaredType, d2: DeclaredType) => d1 == d2 || d1.supertype.exists(isSubtype(_, d2)) },

      // A component type p1 is a subtype of p2 if p1's underlying type is a subtype of p2's underlying type.
      { case (p1: ComponentType, p2: ComponentType) => isSubtype(p1.underlying, p2.underlying) },
      // An entity type e1 is a subtype of a component type p2 if e1 has a component of type p1.underlying that is a
      // subtype of p2.underlying. We shouldn't compare p1/p2 directly (as component types) because the rule following
      // this rule will lead to weird interactions with owned-by types. For example, say we have a component C1 owned
      // by +A. We have entity E with components B and C1. C1 cannot be a component of E, because A is not a component
      // of E. But we'd have:
      //            (2)           (3)                  (substitute)
      //    E <: +A <== +C1 <: +A <== C1.ownedBy <= +A <== +A <= +A
      // This can only occur if we compare component types instead of underlying types directly. Since this rule is
      // supposed to ONLY look at the component's types and not their owned-by types, we defer to "underlying".
      { case (e1: ClassType, p2: ComponentType) if e1.isEntity => e1.componentTypes.exists(p1 => isSubtype(p1.underlying, p2.underlying)) },
      // Let's say p1 = +u. A component type +u is a subtype of t2 if u's owned-by type is a subtype of t2. We know
      // that +u is an entity with the type +u & u.ownedBy, as any entity having the component u must also satisfy
      // its ownership constraint, so if u.ownedBy <= t2, we know that +u <= t2.
      { case (p1: ComponentType, t2) => p1.underlying.ownedBy.exists(ownedBy => isSubtype(ownedBy, t2)) },

      // An intersection type i1 is the subtype of an intersection type i2, if all types in i2 are subsumed by i1.
      { case (i1: IntersectionType, i2: IntersectionType) => i2.types.forall(ic2 => isAnyPartSubtypeOf(i1, ic2)) },
      // An intersection type i1 is the subtype of another type t2 if one component of i1 is a subtype of t2.
      { case (i1: IntersectionType, t2) => isAnyPartSubtypeOf(i1, t2) },
      // A non-intersection type t1 is the subtype of an intersection type i2, if t1 is the subtype of all types in i2.
      { case (t1, i2: IntersectionType) => i2.types.forall(ic2 => isSubtype(t1, ic2)) },

      // TODO: This should be defined over subsets of s2: For example, A | B <= A | B | C, but this does not hold with the current code.
      // A sum type s1 is the subtype of a sum type s2, if all types in s1 are also (possibly supertyped) in s2.
      { case (s1: SumType, s2: SumType) => s1.types.forall(sc1 => s2.types.exists(sc2 => isSubtype(sc1, sc2))) },
      // A type t1 is the subtype of a sum type s2, if t1 is a subtype of any of the types in s2.
      { case (t1, s2: SumType) => s2.types.exists(sc2 => isSubtype(t1, sc2)) },
      // A sum type s1 is the subtype of a non-sum type t2, if all individual types in s1 are subtypes of t2.
      // More formally: A <= C and B <= C implies A | B <= C
      { case (s1: SumType, t2) => s1.types.forall(sc1 => isSubtype(sc1, t2)) },

      // A product type p1 is the subtype of a product type p2 if both types have the same number of elements
      // and each element of p1 is a subtype of the element in p2 that is at the same position.
      {
        case (p1: ProductType, p2: ProductType) =>
          p1.components.size == p2.components.size && p1.components.zip(p2.components).forall {
            case (e1, e2) => isSubtype(e1, e2)
          }
      },

      // Lists are covariant.
      { case (l1: ListType, l2: ListType) => isSubtype(l1.element, l2.element) },

      // Maps are invariant because they are mutable (for now).
      { case (m1: MapType, m2: MapType) => m1.key == m2.key && m1.value == m2.value },

      // Handle basic types. Int is a subtype of Real.
      { case (a: BasicType, b: BasicType) => a eq b },
      { case (BasicType.Int, BasicType.Real) => true },

      // The Any type is supertype of all types.
      { case (_, BasicType.Any) => true },

      // The Nothing type is subtype of all types.
      { case (BasicType.Nothing, _) => true },
    )
  }

  private def isAnyPartSubtypeOf(intersectionType: IntersectionType, candidateSupertype: Type): Boolean = {
    intersectionType.types.exists(t => isSubtype(t, candidateSupertype))
  }

  /**
    * Whether t1 is a subtype of t2.
    */
  def isSubtype(t1: Type, t2: Type): Boolean = {
    t1 == t2 || TypeRelations.inRelation(rules)(t1, t2)
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
