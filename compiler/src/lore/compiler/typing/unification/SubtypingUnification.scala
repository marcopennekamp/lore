package lore.compiler.typing.unification

import lore.compiler.inference.Inference.{Assignments, instantiateByBound, instantiateCandidateType, isFullyInstantiated}
import lore.compiler.inference.InferenceBounds.BoundType
import lore.compiler.inference.{Inference, InferenceVariable}
import lore.compiler.types._
import lore.compiler.typing.InferenceVariable2
import lore.compiler.utils.CollectionExtensions.VectorExtension

object SubtypingUnification {

  /**
    * @param isFit If enabled, both bounds of an inference variable will be assigned.
    */
  def unify(t1: Type, t2: Type, isFit: Boolean, assignments: Assignments): Option[Assignments] = {
    if (isFullyInstantiated(t1) && isFullyInstantiated(t2)) {
      return if (t1 <= t2) Some(assignments) else None
    }

    def unsupported: Option[Assignments] = {
      Inference.logger.debug(s"Subtyping unification of intersection and sum types is not yet supported." +
        s" Given types: `$t1` and `$t2`.")
      None
    }

    def ensure(iv: InferenceVariable, tpe: Type, boundType: BoundType, assignments: Assignments): Option[Assignments] = {
      if (isFit) {
        val candidateType = instantiateCandidateType(assignments, tpe)
        InferenceVariable2.ensure(iv, candidateType, candidateType, assignments)
      } else {
        InferenceVariable2.ensure(iv, instantiateByBound(assignments, tpe, boundType), boundType, assignments)
      }
    }

    (t1, t2) match {
      case (iv1: InferenceVariable, iv2: InferenceVariable) =>
        if (isFit) {
          Unification.unifyEquals(iv1, iv2, assignments)
        } else {
          InferenceVariable2.ensure(iv2, instantiateByBound(assignments, iv1, BoundType.Lower), BoundType.Lower, assignments).flatMap {
            assignments2 => InferenceVariable2.ensure(iv1, instantiateByBound(assignments2, iv2, BoundType.Upper), BoundType.Upper, assignments2)
          }
        }
      case (iv1: InferenceVariable, t2) => ensure(iv1, t2, BoundType.Upper, assignments)
      case (t1, iv2: InferenceVariable) => ensure(iv2, t1, BoundType.Lower, assignments)

      case (t1: TupleType, t2: TupleType) => if (t1.elements.size == t2.elements.size) {
        t1.elements.zip(t2.elements).foldSome(assignments) {
          case (assignments2, (e1, e2)) => unify(e1, e2, isFit, assignments2)
        }
      } else None

      case (f1: FunctionType, f2: FunctionType) => unify(f2.input, f1.input, isFit, assignments).flatMap(unify(f1.output, f2.output, isFit, _))

      case (l1: ListType, l2: ListType) => unify(l1.element, l2.element, isFit, assignments)

      case (m1: MapType, m2: MapType) => unify(m1.key, m2.key, isFit, assignments).flatMap(unify(m1.value, m2.value, isFit, _))

      case (s1: ShapeType, s2: ShapeType) =>
        s2.correlate(s1).foldSome(assignments) {
          case (assignments2, (p2, Some(p1))) => unify(p1.tpe, p2.tpe, isFit, assignments2)
          case (_, (_, None)) => None
        }
      case (d1: DeclaredType, s2: ShapeType) => unify(d1.asShapeType, s2, isFit, assignments)

      case (d1: DeclaredType, d2: DeclaredType) =>
        d1.findSupertype(d2.schema) match {
          case Some(s1) => s1.typeArguments.zip(d2.typeArguments).foldSome(assignments) {
            case (assignments2, (a1, a2)) => unify(a1, a2, isFit, assignments2)
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

}
