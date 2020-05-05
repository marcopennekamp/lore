package lore.types

import lore.compiler.Registry
import scalaz.Monad
import scalaz.std.list._
import scalaz.syntax.traverse._

object Subtyping {

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

    // Int is a subtype of Real.
    { case (BasicType.Int, BasicType.Real) => true },

    // The Any type is supertype of all types.
    { case (_, AnyType) => true },

    // The Nothing type is subtype of all types.
    { case (NothingType, _) => true },
  )

  /**
    * Whether t1 is a supertype of t2.
    */
  def isSupertype(t1: Type, t2: Type): Boolean = isSubtype(t2, t1)

  /**
    * Whether t1 is a subtype of t2.
    */
  def isSubtype(t1: Type, t2: Type): Boolean = {
    // TODO: We might need to use a more complex theorem solver with proper typing rules instead of such an ad-hoc/greedy algorithm.
    // t1 is a subtype of t2 if any of the rules are true.
    for (rule <- subtypingRules) {
      if (rule.isDefinedAt((t1, t2)) && rule.apply((t1, t2))) return true
    }
    false
  }

  /**
    * Whether t1 is a strict supertype of t2.
    */
  def isStrictSupertype(t1: Type, t2: Type): Boolean = t1 != t2 && isSupertype(t1, t2)

  /**
    * Whether t1 is a strict subtype of t2.
    */
  def isStrictSubtype(t1: Type, t2: Type): Boolean = t1 != t2 && isSubtype(t1, t2)

  /**
    * A set of direct subtypes that are resolved IF the given type is abstract.
    *
    * This is an implementation of the 'ards' function as defined in the spec. See the spec for more information.
    */
  def abstractResolvedDirectSubtypes(t: Type)(implicit registry: Registry): Set[Type] = {
    // TODO: Using Set like this (which is much slower than List) could be a major performance hog down the line.
    //       We should watch out for any performance problems stemming from ards evaluation.
    implicit val setMonad: Monad[Set] = new Monad[Set] {
      override def point[A](a: => A): Set[A] = Set(a)
      override def bind[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)
    }
    def combinations(components: List[Set[Type]]) = components.sequence

    t match {
      case _ if !t.isAbstract => Set(t)
      case dt: DeclaredType => dt.directDeclaredSubtypes
      case ProductType(components) => combinations(components.map(abstractResolvedDirectSubtypes)).map(ProductType(_))
      case IntersectionType(types) => combinations(types.map(abstractResolvedDirectSubtypes).toList).map(IntersectionType.construct)
      case SumType(types) => types.flatMap(abstractResolvedDirectSubtypes)
      case _ if t == AnyType =>
        // TODO: Really? This should rather be the set of all types which have no supertype, i.e. direct descendants
        //       of Any. Or maybe, rather, let's not fuck with Any for abstract functions and return an error here.
        Set.empty
      case t if t == NothingType => Set.empty
    }
  }

  /**
    * Calculates the least upper bound of two types, which is the most specific (super-)type that values of both
    * t1 and t2 could be assigned to.
    *
    * In general, we can say that if t1 <= t2, then t2 is the solution to this problem. This is easy to see: We
    * want the most specific common supertype. What could be more specific than one of the types themselves?
    *
    * This question can always be answered with the solution `t1 | t2`. We are not looking for such a solution,
    * UNLESS we are already dealing with sum types. So if we have an IF that is evaluating to A | B in one block
    * and C in another block, we will return the common supertype A | B | C (unless A | B <= C). This follows the
    * most likely user intent. If it doesn't, the problem will be caught at most at function-level, since return
    * types have to be specified anyway. The subtyping rule above is still preferred to this special handling,
    * however.
    */
  def leastUpperBound(t1: Type, t2: Type)(implicit registry: Registry): Type = (t1, t2) match {
    // First of all, handle subtypes as outlined above. These cases trivially cover Any and Nothing.
    case (t1, t2) if t1 <= t2 => t2
    case (t1, t2) if t2 <= t1 => t1

    // We handle sum types specially, as outlined above.
    case (t1: SumType, _) => SumType.construct(Set(t1, t2))
    case (_, t2: SumType) => leastUpperBound(t2, t1) // Delegate to the previous case.

    // Intersection types can lead to a number of candidates. That is, if we have types A & B and C & D, we have
    // the following candidates: LUB(A, C), LUB(A, D), LUB(B, C), LUB(B, D). That's because any component of an
    // intersection type is its supertype, so the least upper bound of two intersection types would be a combination
    // of one component from t1 and another component from t2.
    // From the candidates, we choose the most specific types, that is those who don't have a subtype in the
    // candidate list. Those types are finally structured into an intersection type.
    // TODO: Is this algorithm the same when applied via reduction (reducing to the LUB by pairs) and to a list
    //       of intersection types? That is, does the following hold: LUB(LUB(A, B), C) = LUB(A, B, C)?
    case (t1: IntersectionType, t2: IntersectionType) =>
      val candidates = for  { c1 <- t1.types; c2 <- t2.types } yield leastUpperBound(c1, c2)
      val mostSpecificCandidates = candidates.filter { candidate => !candidates.exists(_ < candidate) }
      IntersectionType.construct(mostSpecificCandidates)
    case (t1: IntersectionType, _) => leastUpperBound(t1, IntersectionType(Set(t2))) // Delegate to the previous case.
    case (_, t2: IntersectionType) => leastUpperBound(t2, t1) // Delegate to the previous case.

    // In case of product types, we can decide the closest common supertype component by component.
    //case (ProductType())

    // For class and label types, the LUB is calculated by the type hierarchy.
    case (d1: DeclaredType, d2: DeclaredType) => registry.declaredTypeHierarchy.leastCommonSupertype(d1, d2)

    // TODO: Component types
    // TODO: List types, map types
    // TODO: Basic types?

    // Handle Int and Real specifically.
    case (BasicType.Int, BasicType.Real) => BasicType.Real
    case (BasicType.Real, BasicType.Int) => BasicType.Real

    // In any other case, we can say that types are inherently incompatible. For example, a product type and
    // a class type could never be unified in this sense. The result is the "last resort" type, the supertype
    // of all types: Any.
    case _ => AnyType // TODO: Or return t1 | t2?
  }

}
