package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.semantics.Registry
import lore.compiler.types.TypeExtensions._

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
    // TODO: How do type variables affect this?

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

    def declaredTypeLcs(d1: DeclaredType, d2: DeclaredType): Type = {
      registry.declaredTypeHierarchy.leastCommonSupertype(d1, d2)
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

      // We have the special case that component types can also be supertypes of entity types.
      // We add these to the LUB resolved by the type hierarchy.
      case (e1: ClassType, e2: ClassType) if e1.isEntity && e2.isEntity =>
        val componentTypes = e1.definition.commonComponentTypes(e2.definition)
        val superclassList = declaredTypeLcs(e1, e2) match {
          // We choose Any as the supertype only if there are no common component types. Otherwise we prefer to
          // calculate a LUB that contains only component types.
          case BasicType.Any if componentTypes.nonEmpty => Nil
          case t => t :: Nil
        }
        IntersectionType.construct(
          superclassList ::: componentTypes
        ).fallbackIfAny

      // For class and label types, the LUB is calculated by the type hierarchy. If the result would be Any, we return
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
      case (d1: DeclaredType, d2: DeclaredType) => declaredTypeLcs(d1, d2).fallbackIfAny

      // Component types simply delegate to declared types.
      case (c1: ComponentType, c2: ComponentType) =>
        registry.declaredTypeHierarchy.leastCommonSupertype(c1.underlying, c2.underlying) match {
          case IntersectionType(types) =>
            // If we have an intersection type as the LCS, there are multiple LCSs. However, only one of them can
            // be a class type. So we filter for that one.
            val classTypes = types.filter(_.isInstanceOf[ClassType])
            if (classTypes.size != 1) throw CompilationException("There can (and must) only be one!")
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
