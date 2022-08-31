package lore.compiler.typing.unification

/*

import lore.compiler.types._
import lore.compiler.typing.InferenceBounds.BoundType
import lore.compiler.typing.{InferenceVariable, Typing}
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.utils.CollectionExtensions.VectorExtension

object SubtypingUnification {

  /**
    * @param isFit If enabled, both bounds of an inference variable will be assigned.
    */
  def unify(t1: Type, t2: Type, isFit: Boolean, assignments: Assignments): Option[Assignments] = {
    val combiner = if (isFit) FitsCombiner else SubtypesCombiner
    unify(combiner)(t1, t2, assignments)
  }

  trait Combiner {
    def unify(iv1: InferenceVariable, iv2: InferenceVariable, assignments: Assignments): Option[Assignments]
    def ensure(iv: InferenceVariable, tpe: Type, boundType: BoundType, assignments: Assignments): Option[Assignments]
    def ifFullyInferred(t1: Type, t2: Type, assignments: Assignments): Option[Assignments] = if (t1 <= t2) Some(assignments) else None
  }

  object SubtypesCombiner extends Combiner {
    override def unify(iv1: InferenceVariable, iv2: InferenceVariable, assignments: Assignments): Option[Assignments] = {
      InferenceVariable.ensure(iv2, InferenceVariable.instantiateByBound(iv1, BoundType.Lower, assignments), BoundType.Lower, assignments).flatMap {
        assignments2 => InferenceVariable.ensure(iv1, InferenceVariable.instantiateByBound(iv2, BoundType.Upper, assignments2), BoundType.Upper, assignments2)
      }
    }

    override def ensure(iv: InferenceVariable, tpe: Type, boundType: BoundType, assignments: Assignments): Option[Assignments] = {
      InferenceVariable.ensure(iv, InferenceVariable.instantiateByBound(tpe, boundType, assignments), boundType, assignments)
    }
  }

  object FitsCombiner extends Combiner {
    override def unify(iv1: InferenceVariable, iv2: InferenceVariable, assignments: Assignments): Option[Assignments] = {
      Unification.unifyEquals(iv1, iv2, assignments)
    }

    override def ensure(iv: InferenceVariable, tpe: Type, boundType: BoundType, assignments: Assignments): Option[Assignments] = {
      val candidateType = InferenceVariable.instantiateCandidate(tpe, assignments)
      InferenceVariable.ensure(iv, candidateType, candidateType, assignments)
    }
  }

  def unify(combiner: Combiner)(t1: Type, t2: Type, assignments: Assignments): Option[Assignments] = {
    if (InferenceVariable.isFullyInstantiated(t1) && InferenceVariable.isFullyInstantiated(t2)) {
      return combiner.ifFullyInferred(t1, t2, assignments)
    }

    def unsupported: Option[Assignments] = {
      Typing.logger.debug(s"Subtyping unification of intersection and sum types is not yet supported." +
        s" Given types: `$t1` and `$t2`.")
      None
    }

    val rec = unify(combiner) _
    (t1, t2) match {
      case (iv1: InferenceVariable, iv2: InferenceVariable) => combiner.unify(iv1, iv2, assignments)
      case (iv1: InferenceVariable, t2) => combiner.ensure(iv1, t2, BoundType.Upper, assignments)
      case (t1, iv2: InferenceVariable) => combiner.ensure(iv2, t1, BoundType.Lower, assignments)

      case (t1: TupleType, t2: TupleType) => if (t1.elements.size == t2.elements.size) {
        t1.elements.zip(t2.elements).foldSome(assignments) {
          case (assignments2, (e1, e2)) => rec(e1, e2, assignments2)
        }
      } else None

      case (f1: FunctionType, f2: FunctionType) => rec(f2.input, f1.input, assignments).flatMap(rec(f1.output, f2.output, _))

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

}

*/
