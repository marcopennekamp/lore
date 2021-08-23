package lore.compiler.inference.matchers

import lore.compiler.core.CompilationException
import lore.compiler.feedback.Reporter
import lore.compiler.feedback.TypingFeedback.EqualTypesExpected
import lore.compiler.inference.Inference.{Assignments, isFullyInstantiated}
import lore.compiler.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.VectorExtension

object EqualityMatcher {

  /**
    * Ensures that `t1` and `t2` are equal:
    *
    *   - If `t1` and `t2` are both fully instantiated, the matcher defers to a simple type equality check.
    *   - Otherwise, if `t1` or `t2` contain inference variables, types matching to an inference variable are processed
    *     with one of the three processing functions. The resulting assignments shall be built such that
    *     `instantiate(t1) == instantiate(t2)`.
    */
  def matchEquals(processor: Matchers.Processor)(
    t1: Type,
    t2: Type,
    assignments: Assignments,
    context: TypingJudgment,
  )(implicit reporter: Reporter): Option[Assignments] = {
    if (isFullyInstantiated(t1) && isFullyInstantiated(t2)) {
      return if (t1 == t2) Some(assignments) else {
        reporter.error(EqualTypesExpected(t1, t2, context))
        None
      }
    }

    def unsupported: Nothing = {
      throw CompilationException(s"Inference equality matching of intersection and sum types is not yet supported." +
        s" Given types: $t1 and $t2.")
    }

    def expectedTypeEquality() = {
      reporter.error(EqualTypesExpected(t1, t2, context))
      None
    }

    val rec = (assignments2: Assignments, u1: Type, u2: Type) => matchEquals(processor)(u1, u2, assignments2, context)
    (t1, t2) match {
      case (iv1: InferenceVariable, iv2: InferenceVariable) => processor.processBoth(iv1, iv2, assignments, context)
      case (iv1: InferenceVariable, t2) => processor.processIv1(iv1, t2, assignments, context)
      case (t1, iv2: InferenceVariable) => processor.processIv2(t1, iv2, assignments, context)

      case (t1: TupleType, t2: TupleType) => Matchers.matchTuple(t1, t2, assignments, rec, expectedTypeEquality)

      case (f1: FunctionType, f2: FunctionType) => rec(assignments, f1.input, f2.input).flatMap(rec(_, f1.output, f2.output))

      case (l1: ListType, l2: ListType) => rec(assignments, l1.element, l2.element)

      case (m1: MapType, m2: MapType) => rec(assignments, m1.key, m2.key).flatMap(rec(_, m1.value, m2.value))

      case (s1: ShapeType, s2: ShapeType) =>
        // Shape types must share all properties to be equal.
        ShapeType.bicorrelate(s1, s2).foldSome(assignments) {
          case (assignments2, (Some(p1), Some(p2))) => rec(assignments2, p1.tpe, p2.tpe)
          case _ => expectedTypeEquality()
        }

      case (d1: DeclaredType, d2: DeclaredType) if d1.schema == d2.schema =>
        Matchers.matchMultiple(d1.typeArguments.zip(d2.typeArguments), assignments, rec)

      case (_: IntersectionType, _) => unsupported
      case (_, _: IntersectionType) => unsupported
      case (_: SumType, _) => unsupported
      case (_, _: SumType) => unsupported

      case _ => expectedTypeEquality()
    }
  }

}
