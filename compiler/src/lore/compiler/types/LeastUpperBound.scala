package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.semantics.Registry
import lore.compiler.types.TypeExtensions._
import lore.compiler.utils.CollectionExtensions.VectorExtension

object LeastUpperBound {

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
    def fallback = if (defaultToSum) SumType.construct(Set(t1, t2)) else BasicType.Any
    def lubPassOnSettings = configurableLub(defaultToSum) _

    implicit class FallbackIfAny(tpe: Type) {
      def fallbackIfAny: Type = tpe match {
        case BasicType.Any => fallback
        case t => t
      }
    }

    (t1, t2) match {
      // First of all, handle subtypes as outlined above. These cases trivially cover Any and Nothing.
      case (t1, t2) if t1 <= t2 => t2
      case (t1, t2) if t2 <= t1 => t1

      // The least upper bound of two type variables is simply the LUB of their upper bounds (at compile-time, of
      // course). All values assignable to either v1 or v2 for ANY actual types that the variables can represent at
      // run-time must be described by the result of the LUB. This can only be either the sum of the two variables,
      // V1 | V2, or preferably some kind of common upper bound.
      case (v1: TypeVariable, v2: TypeVariable) => lubNoDefaultSum(v1.upperBound, v2.upperBound).fallbackIfAny
      // The least upper bound of a type variable and a type (which is not itself a type variable) is similar to the
      // treatment above, only with the idea that we treat the type t2 as its own "upper bound".
      case (v1: TypeVariable, t2) => lubNoDefaultSum(v1.upperBound, t2).fallbackIfAny
      case (t1, v2: TypeVariable) => lubPassOnSettings(v2, t1)

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
        val candidates = for  {c1 <- t1.parts; c2 <- t2.parts} yield lubNoDefaultSum(c1, c2)
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

      // In case of product types, we can decide the closest common supertype element by element.
      case (ProductType(left), ProductType(right)) if left.size == right.size => ProductType(left.zip(right).map(lubPassOnSettings.tupled))

      // For declared types, the LUB is calculated by the type hierarchy. If the result would be Any, we return
      // the sum type instead.
      // TODO: Now that we have schemas, we have to check whether types e1 and e2 assign the same type variables
      //       for their most specific common supertype. Since type variables of a child type do not have to
      //       correspond 1:1 to those of a parent type, we have to follow the "call chain" of supertypes until
      //       we arrive at the desired schema, for both types, while substituting the right types. Only if we
      //       arrive at equal type arguments to the LUB is it actually a common supertype. Since there may be
      //       multiple such types, we have to check it for each independently, and then create an intersection
      //       type (or Any) with the fitting supertypes.
      // TODO: I don't think the above works so naively. Take the following type hierarchy:
      //       abstract class A
      //       abstract class B[T] extends A
      //       class C[T] extends B[T]
      //       class D[T] extends B[T]
      //       The LUB of C[Int] and D[String] should be A, but the algorithm as envisioned would return Any, because
      //       the declared type hierarchy returns only B as the most specific common schema. The way to fix this is
      //       to instantiate a subgraph of the declared type hierarchy with the given type arguments.
      case (d1: DeclaredType, d2: DeclaredType) => registry.declaredTypeHierarchy.leastCommonSupertype(d1, d2).fallbackIfAny

      // Component types mostly delegate to declared types.
      case (c1: ComponentType, c2: ComponentType) =>
        val candidates = registry.declaredTypeHierarchy.leastCommonSupertype(c1.underlying, c2.underlying) match {
          case IntersectionType(types) =>
            // Let's say the underlying types U1 and U2 have the least upper bound A & B. This means that both types
            // can be represented either as A or as B, interchangeably. So if we have an entity e1 with a component +U1
            // and another e2 with a component +U2, they can be represented both as +A and +B. We can say e1.A and refer
            // to U1. We can say e2.A and refer to U2. We can say e1.B and refer to U1... The point being that the
            // common denominator of +U1 and +U2 is that they can be viewed through the lens of both +A and +B. Hence,
            // it is legal to define LUB(+U1, +U2) as +A & +B. This stretches to intersection types with more than two
            // parts.
            types.toVector.filterType[DeclaredType]
          case tpe: DeclaredType => Vector(tpe)
          case BasicType.Any => Vector.empty
          case _ => throw CompilationException(s"The least upper bound of $c1 and $c2 is ill-defined because the least" +
            s" common supertype of the underlying types is neither an intersection type nor a declared type nor Any.")
        }

        // For full correctness, we have to ensure that only ownable declared types can be the underlying types of
        // the new component types.
        val parts = candidates.filter(_.isOwnable)
        if (parts.nonEmpty) IntersectionType.construct(parts.map(ComponentType)) else fallback

      // Lists simply wrap the lubbed types.
      case (ListType(e1), ListType(e2)) => ListType(lubPassOnSettings(e1, e2))

      // Maps also wrap the lubbed types, but for both key and value types.
      case (MapType(k1, v1), MapType(k2, v2)) => MapType(lubPassOnSettings(k1, k2), lubPassOnSettings(v1, v2))

      // Handle Int and Real specifically.
      case (BasicType.Int, BasicType.Real) => BasicType.Real
      case (BasicType.Real, BasicType.Int) => BasicType.Real

      // In any other case, we can say that t1 and t2 are inherently incompatible. For example, a product type and
      // a struct type could never be unified in this sense, and neither could two product types with different
      // lengths. Hence we return the default.
      case _ => fallback
    }
  }

}
