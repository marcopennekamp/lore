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

  def leastUpperBound(t1: Type, t2: Type)(implicit registry: Registry): Type = configurableLub(defaultToSum = true)(t1, t2)
  private def lubNoDefaultSum(t1: Type, t2: Type)(implicit registry: Registry): Type = configurableLub(defaultToSum = false)(t1, t2)

  /**
    * Calculates the least upper bound of two types, which is the most specific (super-)type that values of both
    * t1 and t2 could be assigned to. If t1 and t2 don't share a supertype, we return t1 | t2 or Any, depending on
    * the defaultToSum setting.
    *
    * In general, we can say that if t1 <= t2, then t2 is the solution to this problem. This is easy to see: We
    * want the most specific common supertype. What could be more specific than one of the types themselves?
    */
  private[types] def configurableLub(defaultToSum: Boolean)(t1: Type, t2: Type)(implicit registry: Registry): Type = {
    /**
      * The fallback LUB, which is either Any or t1 | t2 depending on the settings.
      */
    def fallback = if (defaultToSum) SumType.construct(Set(t1, t2)) else AnyType
    def lubPassOnSettings = configurableLub(defaultToSum) _

    implicit class FallbackIfAny(tpe: Type) {
      def fallbackIfAny: Type = tpe match {
        case AnyType => fallback
        case t => t
      }
    }

    (t1, t2) match {
      // First of all, handle subtypes as outlined above. These cases trivially cover Any and Nothing.
      case (t1, t2) if t1 <= t2 => t2
      case (t1, t2) if t2 <= t1 => t1

      // Intersection types can lead to a number of candidates. That is, if we have types A & B and C & D, we have
      // the following candidates: LUB(A, C), LUB(A, D), LUB(B, C), LUB(B, D). That's because any component of an
      // intersection type is its supertype, so the least upper bound of two intersection types would be a combination
      // of one component from t1 and another component from t2.
      // From the candidates, we choose the most specific types, meaning those who don't have a strict subtype in the
      // candidate list. This part of the algorithm is taken care of by IntersectionType.construct, as intersection
      // types are specifically simplified according to this rule.
      // TODO: Is this algorithm the same when applied via reduction (reducing to the LUB by pairs) and to a list
      //       of intersection types? That is, does the following hold: LUB(LUB(A, B), C) = LUB(A, B, C)?
      case (t1: IntersectionType, t2: IntersectionType) =>
        // TODO: When calling lubNoDefaultSum, should we pass the "no default to sum" setting down the call tree,
        //       actually? Or should this be confined to the immediate concern of having a correct candidate algorithm?
        //       We need to test this with more complex examples, though they should still make sense, of course.
        val candidates = for  { c1 <- t1.types; c2 <- t2.types } yield lubNoDefaultSum(c1, c2)
        IntersectionType.construct(candidates)
      case (t1: IntersectionType, _) => lubPassOnSettings(t1, IntersectionType(Set(t2)))
      case (_, t2: IntersectionType) => lubPassOnSettings(t2, t1)

      // In this step, we always try to join sum types.
      case (t1: SumType, _) =>
        lubNoDefaultSum(
          t1.join,
          t2 match { case st: SumType => st.join; case t => t }
        ).fallbackIfAny
      case (_, t2: SumType) => lubPassOnSettings(t2, t1)

      // In case of product types, we can decide the closest common supertype component by component.
      case (ProductType(left), ProductType(right)) =>
        if (left.size != right.size) fallback
        else ProductType(left.zip(right).map(lubPassOnSettings.tupled))

      // For class and label types, the LUB is calculated by the type hierarchy. If the result would be Any, we return
      // the sum type instead.
      case (d1: DeclaredType, d2: DeclaredType) => registry.declaredTypeHierarchy.leastCommonSupertype(d1, d2).fallbackIfAny

      // Component types simply delegate to declared types.
      case (c1: ComponentType, c2: ComponentType) =>
        registry.declaredTypeHierarchy.leastCommonSupertype(c1.underlying, c2.underlying) match {
          case IntersectionType(types) =>
            // If we have an intersection type as the LCS, there are multiple LCSs. However, only one of them can
            // be a class type. So we filter for that one.
            val classTypes = types.filter(_.isInstanceOf[ClassType])
            if (classTypes.size != 1) throw new RuntimeException("There can (and must) only be one! This is a compiler bug.")
            classTypes.head
          case classType: ClassType => ComponentType(classType)
          case t => t.fallbackIfAny
        }

      // Lists simply wrap the lubbed types.
      case (ListType(e1), ListType(e2)) => ListType(lubPassOnSettings(e1, e2))

      // Maps also wrap the lubbed types, but for both key and value types.
      case (MapType(k1, v1), MapType(k2, v2)) => MapType(lubPassOnSettings(k1, k2), lubPassOnSettings(v1, v2))

      // Handle Int and Real specifically.
      case (BasicType.Int, BasicType.Real) => BasicType.Real
      case (BasicType.Real, BasicType.Int) => BasicType.Real

      // In any other case, we can say that t1 and t2 are inherently incompatible. For example, a product type and
      // a class type could never be unified in this sense. Hence we return the default.
      case _ => fallback
    }
  }

}
