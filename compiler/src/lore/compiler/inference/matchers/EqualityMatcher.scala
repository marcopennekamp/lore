package lore.compiler.inference.matchers

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.feedback.TypingFeedback.EqualTypesExpected
import lore.compiler.inference.Inference.{Assignments, isFullyInstantiated}
import lore.compiler.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.types._

object EqualityMatcher {

  /**
    * Ensures that `t1` and `t2` are equal:
    *
    *   - If `t1` and `t2` are both fully instantiated, this resolution defers to a simple type equality check.
    *   - Otherwise, if `t1` or `t2` contain inference variables, types matching to an inference variable are processed
    *     with one of the three processing functions. The resulting assignments shall be built such that
    *     `instantiate(t1) == instantiate(t2)`.
    */
  def matchEquals(
    processIv1: (InferenceVariable, Type, Assignments, TypingJudgment) => Compilation[Assignments],
    processIv2: (Type, InferenceVariable, Assignments, TypingJudgment) => Compilation[Assignments],
    processBoth: (InferenceVariable, InferenceVariable, Assignments, TypingJudgment) => Compilation[Assignments],
  )(t1: Type, t2: Type, assignments: Assignments, context: TypingJudgment): Compilation[Assignments] = {
    if (isFullyInstantiated(t1) && isFullyInstantiated(t2)) {
      return if (t1 == t2) Compilation.succeed(assignments) else Compilation.fail(EqualTypesExpected(t1, t2, context))
    }

    def unsupported: Nothing = {
      throw CompilationException(s"Inference equality matching of intersection and sum types is not yet supported." +
        s" Given types: $t1 and $t2.")
    }

    def expectedTypeEquality = Compilation.fail(EqualTypesExpected(t1, t2, context))

    val rec = (assignments2: Assignments, u1: Type, u2: Type) => matchEquals(processIv1, processIv2, processBoth)(u1, u2, assignments2, context)
    (t1, t2) match {
      case (iv1: InferenceVariable, iv2: InferenceVariable) => processBoth(iv1, iv2, assignments, context)
      case (iv1: InferenceVariable, t2) => processIv1(iv1, t2, assignments, context)
      case (t1, iv2: InferenceVariable) => processIv2(t1, iv2, assignments, context)

      case (p1: ProductType, p2: ProductType) => Matchers.matchTuple(p1, p2, assignments, rec, expectedTypeEquality)

      case (f1: FunctionType, f2: FunctionType) => rec(assignments, f1.input, f2.input).flatMap(rec(_, f1.output, f2.output))

      case (l1: ListType, l2: ListType) => rec(assignments, l1.element, l2.element)

      case (m1: MapType, m2: MapType) => rec(assignments, m1.key, m2.key).flatMap(rec(_, m1.value, m2.value))

      case (s1: ShapeType, s2: ShapeType) =>
        // Shape types must share all properties to be equal.
        ShapeType.bicorrelate(s1, s2).foldLeft(Compilation.succeed(assignments)) {
          case (compilation, (Some(p1), Some(p2))) => compilation.flatMap(rec(_, p1.tpe, p2.tpe))
          case _ => expectedTypeEquality
        }

      case (_: IntersectionType, _) => unsupported
      case (_, _: IntersectionType) => unsupported
      case (_: SumType, _) => unsupported
      case (_, _: SumType) => unsupported

      case _ => expectedTypeEquality
    }
  }

}
