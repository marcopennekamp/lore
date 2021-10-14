package lore.compiler.typing.unification

import lore.compiler.inference.Inference.{Assignments, instantiateByBound, isFullyInstantiated}
import lore.compiler.inference.InferenceBounds.BoundType
import lore.compiler.inference.{Inference, InferenceVariable}
import lore.compiler.types._
import lore.compiler.typing.InferenceVariable2
import lore.compiler.utils.CollectionExtensions.VectorExtension

object EqualityUnification {

  /**
    * Unifies `t1` and `t2` such that `t1` is equal to `t2` in the given `boundTypes`, assigning inference variables
    * accordingly.
    *
    * If the types cannot be unified, `None` is returned. Unification does not report errors on its own.
    */
  def unify(t1: Type, t2: Type, boundTypes: Vector[BoundType], assignments: Assignments): Option[Assignments] = {
    if (isFullyInstantiated(t1) && isFullyInstantiated(t2)) {
      return if (t1 == t2) Some(assignments) else None
    }

    def unsupported: Option[Assignments] = {
      Inference.logger.debug(s"Equality unification of intersection and sum types is not yet supported." +
        s" Given types: `$t1` and `$t2`.")
      None
    }

    (t1, t2) match {
      case (iv1: InferenceVariable, iv2: InferenceVariable) => unifyInferenceVariables(iv1, iv2, assignments, boundTypes)
      case (iv1: InferenceVariable, t2) => unifyInferenceVariableWithType(iv1, t2, boundTypes, assignments)
      case (t1, iv2: InferenceVariable) => unifyInferenceVariableWithType(iv2, t1, boundTypes, assignments)

      case (t1: TupleType, t2: TupleType) => if (t1.elements.size == t2.elements.size) {
        t1.elements.zip(t2.elements).foldSome(assignments) {
          case (assignments2, (e1, e2)) => unify(e1, e2, boundTypes, assignments2)
        }
      } else None

      case (f1: FunctionType, f2: FunctionType) => unify(f1.input, f2.input, boundTypes, assignments).flatMap(unify(f1.output, f2.output, boundTypes, _))

      case (l1: ListType, l2: ListType) => unify(l1.element, l2.element, boundTypes, assignments)

      case (m1: MapType, m2: MapType) => unify(m1.key, m2.key, boundTypes, assignments).flatMap(unify(m1.value, m2.value, boundTypes, _))

      case (s1: ShapeType, s2: ShapeType) =>
        // Shape types must share all properties to be equal.
        ShapeType.bicorrelate(s1, s2).foldSome(assignments) {
          case (assignments2, (Some(p1), Some(p2))) => unify(p1.tpe, p2.tpe, boundTypes, assignments2)
          case _ => None
        }

      case (d1: DeclaredType, d2: DeclaredType) if d1.schema == d2.schema =>
        d1.typeArguments.zip(d2.typeArguments).foldSome(assignments) {
          case (assignments2, (a1, a2)) => unify(a1, a2, boundTypes, assignments2)
        }

      // TODO (inference): We might have to support the cases where an intersection type only stands on the left, like
      //                   in TypeVariableAllocation. This also hinges on the TODO mentioned in TypeVariableAllocation.
      case (_: IntersectionType, _) => unsupported
      case (_, _: IntersectionType) => unsupported
      case (_: SumType, _) => unsupported
      case (_, _: SumType) => unsupported

      case _ => None
    }
  }

  /**
    * Unify the bounds of `iv1` and `iv2` such that their instantiated versions are equal in the given `boundTypes`.
    */
  private def unifyInferenceVariables(
    iv1: InferenceVariable,
    iv2: InferenceVariable,
    assignments: Assignments,
    boundTypes: Vector[BoundType],
  ): Option[Assignments] = {
    val bounds1 = InferenceVariable.bounds(iv1, assignments)
    val bounds2 = InferenceVariable.bounds(iv2, assignments)

    def assignBoth(bound: Type, boundType: BoundType, assignments: Assignments) = {
      InferenceVariable2.assign(iv1, bound, boundType, assignments)
        .flatMap(InferenceVariable2.assign(iv2, bound, boundType, _))
    }

    val lowerAssignments = if (boundTypes.contains(BoundType.Lower)) {
      assignBoth(SumType.construct(bounds1.lower, bounds2.lower), BoundType.Lower, assignments)
    } else Some(assignments)

    lowerAssignments.flatMap { lowerAssignments =>
      assignBoth(IntersectionType.construct(bounds1.upper, bounds2.upper), BoundType.Upper, lowerAssignments)
    }
  }

  /**
    * Unify `iv` with `tpe` such that their instantiated versions are equal in the given `boundTypes`. This might
    * assign new bounds to `iv`, or to the inference variables contained in `tpe`. Which path is chosen depends on
    * whether an instantiated `tpe` is narrower than an instantiated `iv`.
    *
    * For example, consider `tpe = (iv2, iv3)`. If `iv = (A, B)`, but `tpe` would be instantiated as
    * `(Nothing, Nothing)` and `(Any, Any)`, we have to assign `(A, B)` to `(iv2, iv3)`, as the instantiated `tpe` is
    * wider than the instantiated `iv`. Conversely, if `iv = (iv4, iv5)`, and `tpe` would be instantiated as `(A, B)`,
    * we have to assign `(A, B)` to `(iv4, iv5)`.
    */
  private def unifyInferenceVariableWithType(
    iv: InferenceVariable,
    tpe: Type,
    boundTypes: Vector[BoundType],
    assignments: Assignments,
  ): Option[Assignments] = {
    def narrowByBound(assignments: Assignments, boundType: BoundType) = {
      if (boundTypes.contains(boundType)) {
        val instantiatedIv = instantiateByBound(assignments, iv, boundType)
        val instantiatedType = instantiateByBound(assignments, tpe, boundType)
        lazy val isTypeNarrower = boundType match {
          case BoundType.Lower => instantiatedIv <= instantiatedType
          case BoundType.Upper => instantiatedType <= instantiatedIv
        }
        if (!isFullyInstantiated(tpe) && !isTypeNarrower) {
          unify(instantiatedIv, tpe, Vector(boundType), assignments)
        } else {
          InferenceVariable2.assign(iv, instantiatedType, boundType, assignments)
        }
      } else Some(assignments)
    }

    narrowByBound(assignments, BoundType.Lower).flatMap(narrowByBound(_, BoundType.Upper))
  }

}
