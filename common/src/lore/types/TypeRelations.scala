package lore.types

/**
  * This object supports calculating arbitrary binary relations between types.
  */
object TypeRelations {

  /**
    * We define the calculation of a type relation in terms of rules that possibly match a pair of types
    * and then decides whether these types are in the relation or not. We define rules as partial functions
    * so that we can have rules with preconditions.
    */
  type Rule = PartialFunction[(Type, Type), Boolean]

  /**
    * Decides whether t1 and t2 are in the relation described by the given rules.
    */
  def inRelation(rules: List[Rule])(t1: Type, t2: Type): Boolean = {
    // TODO: Hide this behind a feature switch so that it doesn't get run or even compiled when we need performance.
    //       This is only for reporting compiler bugs, really.
    if (!rules.exists(_.isDefinedAt((t1, t2)))) {
      println(s"A decision about a relation between types $t1 and $t2 was attempted, but none of the rules match.")
    }

    // t1 is a subtype of t2 if any of the rules are true.
    rules.exists(rule => rule.isDefinedAt((t1, t2)) && rule.apply((t1, t2)))

    // TODO: We might need to use a more complex theorem solver with proper typing rules instead of such an ad-hoc/greedy algorithm.
    //       This is actually working so far, though, and we need it to be fast because of the runtime reality.
  }

  /**
    * Returns all subtyping rules for monomorphic types. This is used by both polymorphic subtyping and
    * assignability.
    *
    * @param isSubtype Decides the subtyping/assignability relationship for a different pair of types.
    */
  def monomorphicSubtypingRules(
    isSubtype: (Type, Type) => Boolean,
    isEqual: (Type, Type) => Boolean,
  ): List[Rule] = {
    def isAnyComponentSubtypeOf(intersectionType: IntersectionType, candidateSupertype: Type): Boolean = {
      intersectionType.types.exists(t => isSubtype(t, candidateSupertype))
    }

    List(
      // A declared type d1 is a subtype of d2 if d1 and d2 are equal or any of d1's hierarchical supertypes equal d2.
      // TODO: Once we have introduced covariant (and possibly contravariant) classes, we will additionally have to
      //       check whether d1's typeArguments are a subtype of d2's type arguments.
      // TODO: Once we introduce parametric declared types, we might have to move this rule out of here.
      { case (d1: DeclaredType, d2: DeclaredType) => d1 == d2 || d1.supertype.exists(isSubtype(_, d2)) },

      // A component type p1 is a subtype of p2 if p1's underlying type is a subtype of p2's underlying type.
      { case (p1: ComponentType, p2: ComponentType) => isSubtype(p1.underlying, p2.underlying) },
      // An entity type e1 is a subtype of a component type p2 if e1 has a component type p1 that is a subtype of p2.
      { case (e1: ClassType, p2: ComponentType) if e1.isEntity => e1.componentTypes.exists(p1 => isSubtype(p1, p2)) },

      // An intersection type i1 is the subtype of an intersection type i2, if all types in i2 are subsumed by i1.
      { case (i1: IntersectionType, i2: IntersectionType) => i2.types.forall(ic2 => isAnyComponentSubtypeOf(i1, ic2)) },
      // An intersection type i1 is the subtype of another type t2 if one component of i1 is a subtype of t2.
      { case (i1: IntersectionType, t2) => isAnyComponentSubtypeOf(i1, t2) },
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

      // A product type tt1 is the subtype of a product type tt2, if both types have the same number of components
      // and each component of tt1 is a subtype of the component in tt2 that is at the same position.
      {
        case (p1: ProductType, p2: ProductType) =>
          p1.components.size == p2.components.size && p1.components.zip(p2.components).forall {
            case (c1, c2) => isSubtype(c1, c2)
          }
      },

      // Lists are covariant.
      { case (l1: ListType, l2: ListType) => isSubtype(l1.element, l2.element) },

      // Maps are invariant because they are mutable (for now).
      { case (m1: MapType, m2: MapType) => isEqual(m1.key, m2.key) && isEqual(m1.value, m2.value) },

      // Handle basic types. Int is a subtype of Real.
      { case (a: BasicType, b: BasicType) => a eq b },
      { case (BasicType.Int, BasicType.Real) => true },

      // The Any type is supertype of all types.
      { case (_, AnyType) => true },

      // The Nothing type is subtype of all types.
      { case (NothingType, _) => true },
    )
  }

}
