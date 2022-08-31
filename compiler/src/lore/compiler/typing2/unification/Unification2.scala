package lore.compiler.typing2.unification

import lore.compiler.types._
import lore.compiler.typing2.Typing2
import lore.compiler.typing2.unification.InferenceBounds2.BoundType2
import lore.compiler.utils.CollectionExtensions.VectorExtension

object Unification2 {

  /**
    * Unifies `t1` and `t2` such that `t1` is a subtype of `t2`, assigning inference variables accordingly.
    *
    * If the types cannot be unified, `None` is returned. Unification does not report errors on its own.
    */
  def unifySubtypes(t1: Type, t2: Type, assignments: InferenceAssignments): Option[InferenceAssignments] = {
    unifySubtypes(SubtypesCombiner)(t1, t2, assignments)
  }

  def unifySubtypes(
    ts1: Vector[Type],
    ts2: Vector[Type],
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    unifySubtypes(TupleType(ts1), TupleType(ts2), assignments)
  }

  /**
    * Unifies `t1` and `t2` such that `t1` is a subtype of `t2`, assigning inference variables accordingly. In contrast
    * to [[unifySubtypes]], `unifyFits` assigns both bounds to fix inference variable bounds immediately.
    *
    * This approach is necessary to correctly assign type parameters whose instantiation will be `Nothing` or `Any`.
    * For example, if we have an argument of type `None` (i.e. an `Option[Nothing]`) and a parameter of type
    * `Option[<A>]`, we want `<A>` to be typed as `(Nothing, Nothing)`, not `(Nothing, Any)`, as this will make it
    * impossible to instantiate the correct candidate type.
    *
    * If the types cannot be unified, `None` is returned. Unification does not report errors on its own.
    */
  def unifyFits(t1: Type, t2: Type, assignments: InferenceAssignments): Option[InferenceAssignments] = {
    unifySubtypes(FitsCombiner)(t1, t2, assignments)
  }

  def unifyFits(
    ts1: Vector[Type],
    ts2: Vector[Type],
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    unifyFits(TupleType(ts1), TupleType(ts2), assignments)
  }

  /**
    * [[unifyInferenceVariableBounds]] unifies the inference variable's current candidate type with the bounds of its
    * respective type variable.
    */
  def unifyInferenceVariableBounds(
    iv: InferenceVariable2,
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    val assignments2 = if (iv.lowerBound != BasicType.Nothing) {
      Unification2.unifySubtypes(iv.lowerBound, iv, assignments).getOrElse(return None)
    } else assignments

    if (iv.upperBound != BasicType.Any) {
      Unification2.unifySubtypes(iv, iv.upperBound, assignments2)
    } else Some(assignments2)
  }

  def unifyInferenceVariableBounds(
    ivs: Vector[InferenceVariable2],
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    ivs.foldSome(assignments) {
      case (assignments, iv) => unifyInferenceVariableBounds(iv, assignments)
    }
  }

  trait SubtypingCombiner {
    def unify(
      iv1: InferenceVariable2,
      iv2: InferenceVariable2,
      assignments: InferenceAssignments,
    ): Option[InferenceAssignments]

    def ensure(
      iv: InferenceVariable2,
      tpe: Type,
      boundType: BoundType2,
      assignments: InferenceAssignments,
    ): Option[InferenceAssignments]

    def ifFullyInferred(
      t1: Type,
      t2: Type,
      assignments: InferenceAssignments,
    ): Option[InferenceAssignments] = {
      if (t1 <= t2) Some(assignments) else None
    }
  }

  object SubtypesCombiner extends SubtypingCombiner {
    override def unify(
      iv1: InferenceVariable2,
      iv2: InferenceVariable2,
      assignments: InferenceAssignments,
    ): Option[InferenceAssignments] = {
      InferenceVariable2.ensure(
        iv2,
        InferenceVariable2.instantiateByBound(iv1, BoundType2.Lower, assignments),
        BoundType2.Lower,
        assignments,
      ).flatMap { assignments2 =>
        InferenceVariable2.ensure(
          iv1,
          InferenceVariable2.instantiateByBound(iv2, BoundType2.Upper, assignments2),
          BoundType2.Upper,
          assignments2,
        )
      }
    }

    override def ensure(
      iv: InferenceVariable2,
      tpe: Type,
      boundType: BoundType2,
      assignments: InferenceAssignments,
    ): Option[InferenceAssignments] = {
      InferenceVariable2.ensure(
        iv,
        InferenceVariable2.instantiateByBound(tpe, boundType, assignments),
        boundType,
        assignments,
      )
    }
  }

  object FitsCombiner extends SubtypingCombiner {
    override def unify(
      iv1: InferenceVariable2,
      iv2: InferenceVariable2,
      assignments: InferenceAssignments,
    ): Option[InferenceAssignments] = {
      unifyInferenceVariablesEqual(
        iv1,
        iv2,
        Vector(BoundType2.Lower, BoundType2.Upper),
        assignments,
      )
    }

    override def ensure(
      iv: InferenceVariable2,
      tpe: Type,
      boundType: BoundType2,
      assignments: InferenceAssignments,
    ): Option[InferenceAssignments] = {
      val candidateType = InferenceVariable2.instantiateCandidate(tpe, assignments)
      InferenceVariable2.ensure(iv, candidateType, candidateType, assignments)
    }
  }

  def unifySubtypes(combiner: SubtypingCombiner)(
    t1: Type,
    t2: Type,
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    if (InferenceVariable2.isFullyInstantiated(t1) && InferenceVariable2.isFullyInstantiated(t2)) {
      return combiner.ifFullyInferred(t1, t2, assignments)
    }

    def unsupported: Option[InferenceAssignments] = {
      Typing2.logger.debug(s"Subtyping unification of intersection and sum types is not yet supported." +
        s" Given types: `$t1` and `$t2`.")
      None
    }

    val rec = unifySubtypes(combiner) _
    (t1, t2) match {
      case (iv1: InferenceVariable2, iv2: InferenceVariable2) => combiner.unify(iv1, iv2, assignments)
      case (iv1: InferenceVariable2, t2) => combiner.ensure(iv1, t2, BoundType2.Upper, assignments)
      case (t1, iv2: InferenceVariable2) => combiner.ensure(iv2, t1, BoundType2.Lower, assignments)

      case (t1: TupleType, t2: TupleType) => if (t1.elements.size == t2.elements.size) {
        t1.elements.zip(t2.elements).foldSome(assignments) {
          case (assignments2, (e1, e2)) => rec(e1, e2, assignments2)
        }
      } else None

      case (f1: FunctionType, f2: FunctionType) =>
        rec(f2.input, f1.input, assignments).flatMap(rec(f1.output, f2.output, _))

      case (l1: ListType, l2: ListType) => rec(l1.element, l2.element, assignments)

      case (m1: MapType, m2: MapType) => rec(m1.key, m2.key, assignments).flatMap(rec(m1.value, m2.value, _))

      case (s1: ShapeType, s2: ShapeType) =>
        s2.correlate(s1).foldSome(assignments) {
          case (assignments2, (p2, Some(p1))) => rec(p1.tpe, p2.tpe, assignments2)
          case (_, (_, None)) => None
        }
      case (d1: DeclaredType, s2: ShapeType) => rec(d1.asShapeType, s2, assignments)

      case (d1: DeclaredType, d2: DeclaredType) =>
        d1.findSupertype(d2.schema) match {
          case Some(s1) => s1.typeArguments.zip(d2.typeArguments).foldSome(assignments) {
            case (assignments2, (a1, a2)) => rec(a1, a2, assignments2)
          }
          case None => None
        }

      case (_: IntersectionType, _) => unsupported
      case (_, _: IntersectionType) => unsupported
      case (_: SumType, _) => unsupported
      case (_, _: SumType) => unsupported

      // Decides subtype relationships such as `Nothing <= [iv]`, where the Nothing cannot be matched to the type on
      // the right-hand side. This would ordinarily be caught by the "fully instantiated" type check above when both t1
      // and t2 are fully instantiated, but not when t1 or t2 contain inference variables.
      case (BasicType.Nothing, _) => Some(assignments)
      case (_, BasicType.Any) => Some(assignments)

      case _ => None
    }
  }

  /**
    * Unify the bounds of `iv1` and `iv2` such that their instantiated versions are equal in the given `boundTypes`.
    */
  private def unifyInferenceVariablesEqual(
    iv1: InferenceVariable2,
    iv2: InferenceVariable2,
    boundTypes: Vector[BoundType2],
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    val bounds1 = InferenceVariable2.getBounds(iv1, assignments)
    val bounds2 = InferenceVariable2.getBounds(iv2, assignments)

    def assignBoth(bound: Type, boundType: BoundType2, assignments: InferenceAssignments) = {
      InferenceVariable2
        .assign(iv1, bound, boundType, assignments)
        .flatMap(InferenceVariable2.assign(iv2, bound, boundType, _))
    }

    val lowerAssignments = if (boundTypes.contains(BoundType2.Lower)) {
      assignBoth(SumType.construct(bounds1.lower, bounds2.lower), BoundType2.Lower, assignments)
    } else Some(assignments)

    lowerAssignments.flatMap { lowerAssignments =>
      assignBoth(IntersectionType.construct(bounds1.upper, bounds2.upper), BoundType2.Upper, lowerAssignments)
    }
  }

}
