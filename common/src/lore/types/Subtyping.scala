package lore.types

trait Subtyping {
  /**
    * We define subtyping like this so that we can test all rules when the pattern matches multiple rules.
    * The alternative would be using a case expression, which would greedily hone in on the first pattern
    * match, unless we use guards, which would make the code quite messy.
    */
  val subtypingRules: Seq[PartialFunction[(Type, Type), Boolean]] = Seq(
    // A declared type d1 is a subtype of d2 if d1 and d2 are equal or any of d1's hierarchical supertypes equal to d2.
    { case (d1: DeclaredType, d2: DeclaredType) =>  d1 == d2 || isSubtype(d1.supertype.getOrElse(AnyType), d2) },

    // A component type p1 is a subtype of p2 if p1's underlying type is a subtype of p2's underlying type.
    { case (p1: ComponentType, p2: ComponentType) => isSubtype(p1.underlying, p2.underlying) },
    // An entity type e1 is a subtype of a component type p2 if e1 has a component type p1 that is a subtype of p2.
    { case (e1: ClassType, p2: ComponentType) if e1.isEntity => e1.componentTypes.exists(p1 => isSubtype(p1, p2)) },

    // An intersection type i1 is the subtype of an intersection type i2, if all types in i2 are subsumed by i1.
    { case (i1: IntersectionType, i2: IntersectionType) => i2.types.forall(ic2 => i1.isAnyComponentSubtypeOf(ic2)) },
    // An intersection type i1 is the subtype of another type t2 if one component of i1 is a subtype of t2.
    { case (i1: IntersectionType, t2) => i1.isAnyComponentSubtypeOf(t2) },
    // A non-intersection type t1 is the subtype of an intersection type i2, if t1 is the subtype of all types in i2.
    { case (t1, i2: IntersectionType) => i2.types.forall(ic2 => isSubtype(t1, ic2)) },

    // A sum type s1 is the subtype of a sum type s2, if all types in s1 are also (possibly supertyped) in s2.
    { case (s1: SumType, s2: SumType) => s1.types.forall(sc1 => s2.types.exists(sc2 => isSubtype(sc1, sc2))) },
    // A type t1 is the subtype of a sum type s2, if t1 is a subtype of any of the types in s2.
    // TODO: This should be defined over subsets of s2: For example, A | B <= A | B | C, but this does not hold with the current code.
    { case (t1, s2: SumType) => s2.types.exists(sc2 => isSubtype(t1, sc2)) },
    // A sum type s1 is the subtype of a non-sum type t2, if all individual types in s1 are subtypes of t2.
    // More formally: A <= C and B <= C implies A | B <= C
    { case (s1: SumType, t2) => s1.types.forall(sc1 => isSubtype(sc1, t2)) },

    // A product type tt1 is the subtype of a product type tt2, if both types have the same number of component types
    // and each component type of tt1 is a subtype of the component type in tt2 that is at the same position.
    {
      case (tt1: ProductType, tt2: ProductType) =>
        tt1.components.size == tt2.components.size && tt1.components.zip(tt2.components).forall {
          case (ttc1, ttc2) => isSubtype(ttc1, ttc2)
        }
    },

    // Lists are covariant.
    { case (l1: ListType, l2: ListType) => isSubtype(l1.element, l2.element) },

    // Handle basic types. Int is a subtype of Real.
    { case (a: BasicType, b: BasicType) => a eq b },
    { case (BasicType.Int, BasicType.Real) => true },

    // The Any type is supertype of all types.
    { case (_, AnyType) => true },

    // The Nothing type is subtype of all types.
    { case (NothingType, _) => true },

    // Subtyping with parametric types only happens with multi-functions. If we have a parametric type here,
    // it is clear that the type variable hasn't been set yet, and so we can only assume its type bounds.
  )

  /**
    * Whether t1 is a subtype of t2.
    */
  def isSubtype(t1: Type, t2: Type): Boolean = {
    // TODO: We might need to use a more complex theorem solver with proper typing rules instead of such an ad-hoc/greedy algorithm.
    //       This is actually working so far, though, and we need it to be fast because that the runtime reality.
    // t1 is a subtype of t2 if any of the rules are true.
    for (rule <- subtypingRules) {
      if (rule.isDefinedAt((t1, t2)) && rule.apply((t1, t2))) return true
    }
    false
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
