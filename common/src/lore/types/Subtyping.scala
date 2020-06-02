package lore.types

trait Subtyping {

  /**
    * We define individual subtyping rules as partial functions so that we can try out multiple rules, instead
    * of relying on a match-case expression that hones in on the first rule only.
    */
  type Rule = PartialFunction[(Type, Type), Boolean]

  /**
    * Returns all subtyping rules for monomorphic types. This is used by both polymorphic subtyping and
    * assignability.
    *
    * @param recurse Decides the subtyping/assignability relationship for a different pair of types.
    */
  def monomorphicRules(recurse: (Type, Type) => Boolean): Seq[Rule] = {
    def isAnyComponentSubtypeOf(intersectionType: IntersectionType, candidateSupertype: Type): Boolean = {
      intersectionType.types.exists(t => recurse(t, candidateSupertype))
    }

    Seq(
      // A declared type d1 is a subtype of d2 if d1 and d2 are equal or any of d1's hierarchical supertypes equal d2.
      // TODO: Once we have introduced covariant (and possibly contravariant) classes, we will additionally have to
      //       check whether d1's typeArguments are a subtype of d2's type arguments.
      // TODO: Once we introduce parametric declared types, we might have to move this rule out of here.
      { case (d1: DeclaredType, d2: DeclaredType) =>  d1 == d2 || recurse(d1.supertype.getOrElse(AnyType), d2) },

      // A component type p1 is a subtype of p2 if p1's underlying type is a subtype of p2's underlying type.
      { case (p1: ComponentType, p2: ComponentType) => recurse(p1.underlying, p2.underlying) },
      // An entity type e1 is a subtype of a component type p2 if e1 has a component type p1 that is a subtype of p2.
      { case (e1: ClassType, p2: ComponentType) if e1.isEntity => e1.componentTypes.exists(p1 => recurse(p1, p2)) },

      // An intersection type i1 is the subtype of an intersection type i2, if all types in i2 are subsumed by i1.
      { case (i1: IntersectionType, i2: IntersectionType) => i2.types.forall(ic2 => isAnyComponentSubtypeOf(i1, ic2)) },
      // An intersection type i1 is the subtype of another type t2 if one component of i1 is a subtype of t2.
      { case (i1: IntersectionType, t2) => isAnyComponentSubtypeOf(i1, t2) },
      // A non-intersection type t1 is the subtype of an intersection type i2, if t1 is the subtype of all types in i2.
      { case (t1, i2: IntersectionType) => i2.types.forall(ic2 => recurse(t1, ic2)) },

      // A sum type s1 is the subtype of a sum type s2, if all types in s1 are also (possibly supertyped) in s2.
      { case (s1: SumType, s2: SumType) => s1.types.forall(sc1 => s2.types.exists(sc2 => recurse(sc1, sc2))) },
      // A type t1 is the subtype of a sum type s2, if t1 is a subtype of any of the types in s2.
      // TODO: This should be defined over subsets of s2: For example, A | B <= A | B | C, but this does not hold with the current code.
      { case (t1, s2: SumType) => s2.types.exists(sc2 => recurse(t1, sc2)) },
      // A sum type s1 is the subtype of a non-sum type t2, if all individual types in s1 are subtypes of t2.
      // More formally: A <= C and B <= C implies A | B <= C
      { case (s1: SumType, t2) => s1.types.forall(sc1 => recurse(sc1, t2)) },

      // A product type tt1 is the subtype of a product type tt2, if both types have the same number of components
      // and each component of tt1 is a subtype of the component in tt2 that is at the same position.
      {
        case (p1: ProductType, p2: ProductType) =>
          p1.components.size == p2.components.size && p1.components.zip(p2.components).forall {
            case (c1, c2) => recurse(c1, c2)
          }
      },

      // Lists are covariant.
      { case (l1: ListType, l2: ListType) => recurse(l1.element, l2.element) },

      // Handle basic types. Int is a subtype of Real.
      { case (a: BasicType, b: BasicType) => a eq b },
      { case (BasicType.Int, BasicType.Real) => true },

      // The Any type is supertype of all types.
      { case (_, AnyType) => true },

      // The Nothing type is subtype of all types.
      { case (NothingType, _) => true },
    )
  }

  def polymorphicRules: Seq[Rule] = Seq[Rule](
    // Two variables are subtypes of each other if v1's upper bound agrees with v2's lower bound. This is easy to
    // see: We want all instances of v1 to be subtypes of all instances of v2. The least specific instance of v1
    // is its upper bound. The most specific instance of v2 is its lower bound. Hence we require these to meet.
    { case (v1: TypeVariable, v2: TypeVariable) => isSubtype(v1.upperBound, v2.lowerBound) },
    // All instances of v1 are definitely subtypes of t2 if v1's upper bound is a subtype of t2, hence ensuring
    // that any instance of v1 has t2 as a supertype.
    { case (v1: TypeVariable, t2) => isSubtype(v1.upperBound, t2) },
    // t1 is definitely a subtype of all instances of v2 if v2's lower bound ensures that instances of v2 are always
    // a supertype of t1.
    { case (t1, v2: TypeVariable) => isSubtype(t1, v2.lowerBound) },
  ) ++ monomorphicRules(isSubtype)

  /**
    * Whether t1 is a subtype of t2.
    *
    * To check subtyping with parametric types, we have to ensure two properties:
    *   1. t1 is a subtype of t2 when parametric types are viewed as their type bounds.
    *   2. Assignments from t1 to type variables of t2 are consistent.
    */
  def isSubtype(t1: Type, t2: Type): Boolean = {
    // If t2 is parametric, we have to check that assignments to its type variables are consistent. This effectively
    // ensures that all instances of the type variable are assigned the SAME type.
    if (t2.isParametric) {
      val assignments: Assignments = Assignments.of(t1, t2)
      if (!assignments.isConsistent) {
        println("Inconsistent type variable assignments.")
        return false
      }
    }

    // TODO: We might need to use a more complex theorem solver with proper typing rules instead of such an ad-hoc/greedy algorithm.
    //       This is actually working so far, though, and we need it to be fast because of the runtime reality.
    // t1 is a subtype of t2 if any of the rules are true.
    for (rule <- polymorphicRules) {
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
