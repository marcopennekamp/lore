package lore.types

import lore.execution.Context
import scalaz.Monad
import scalaz.std.list._
import scalaz.syntax.traverse._

object Subtyping {

  /**
    * We define subtyping like this so that we can test all rules when the pattern matches multiple rules.
    * The alternative would be using a case expression, which would greedily hone in on the first pattern
    * match, unless we use guards, which would make the code quite messy.
    */
  val rules: Seq[PartialFunction[(Type, Type), Boolean]] = Seq(
    // A label type l1 is a subtype of l2 if l1 and l2 are equal or any of l1's supertypes (in line) are equal to l2.
    { case (l1: LabelType, l2: LabelType) =>  l1 == l2 || isSubtype(l1.supertype, l2) },
    // Class types are handled in the same way as label types.
    { case (c1: ClassType, c2: ClassType) => c1 == c2 || isSubtype(c1.supertype, c2) },

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

    // The any type is subtype of none (except itself) and supertype of all types.
    { case (_, AnyType) => true },
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
    // TODO: We can easily optimise this by looping over all rules and returning true when the first rule returns true.
    rules.map(rule => rule.isDefinedAt((t1, t2)) && rule.apply((t1, t2))).exists(identity)
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
  def abstractResolvedDirectSubtypes(t: Type)(implicit context: Context): Set[Type] = {
    implicit val setMonad: Monad[Set] = new Monad[Set] {
      override def point[A](a: => A): Set[A] = Set(a)
      override def bind[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)
    }
    def combinations(components: List[Set[Type]]) = components.sequence.toSet

    t match {
      case _ if !t.isAbstract => Set(t)
      case dt: DeclaredType => dt.directDeclaredSubtypes
      case ProductType(components) => combinations(components.map(abstractResolvedDirectSubtypes)).map(ProductType)
      case IntersectionType(types) => combinations(types.map(abstractResolvedDirectSubtypes).toList).map(IntersectionType.construct)
      case SumType(types) => types.flatMap(abstractResolvedDirectSubtypes)
      // TODO: Really? This should rather be the set of all types which have no supertype, i.e. direct descendants of Any.
      //       Or maybe, rather, let's not fuck with Any for abstract functions.
      case _ if t == AnyType => Set.empty
    }
  }

}
