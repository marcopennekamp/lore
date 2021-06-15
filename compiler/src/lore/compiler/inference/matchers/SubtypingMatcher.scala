package lore.compiler.inference.matchers

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.feedback.TypingFeedback.SubtypeExpected
import lore.compiler.inference.Inference.{Assignments, isFullyInstantiated}
import lore.compiler.inference.{Inference, InferenceVariable, TypingJudgment}
import lore.compiler.types._

object SubtypingMatcher {

  /**
    * Ensures that `t1` is a subtype of `t2`:
    *
    *   - If `t1` and `t2` are both fully instantiated, the matcher defers to a subtyping check.
    *   - Otherwise, if `t1` or `t2` contain inference variables, types matching to an inference variable are processed
    *     with either `process1` or `process2`. The resulting assignments shall be built such that
    *     `instantiate(t1) <= instantiate(t2)`.
    */
  def matchSubtype(
    processIv1: (InferenceVariable, Type, Assignments, TypingJudgment) => Compilation[Assignments],
    processIv2: (Type, InferenceVariable, Assignments, TypingJudgment) => Compilation[Assignments],
    processBoth: (InferenceVariable, InferenceVariable, Assignments, TypingJudgment) => Compilation[Assignments],
  )(t1: Type, t2: Type, assignments: Assignments, context: TypingJudgment): Compilation[Assignments] = {
    if (isFullyInstantiated(t1) && isFullyInstantiated(t2)) {
      return if (t1 <= t2) Compilation.succeed(assignments) else Compilation.fail(SubtypeExpected(t1, t2, context))
    }

    def unsupported: Nothing = {
      throw CompilationException(s"Inference subtype matching of intersection and sum types is not yet supported." +
        s" Given types: $t1 and $t2.")
    }

    def expectedSubtype = Compilation.fail(SubtypeExpected(t1, t2, context))

    val rec = (newAssignments: Assignments, u1: Type, u2: Type) => matchSubtype(processIv1, processIv2, processBoth)(u1, u2, newAssignments, context)
    (t1, t2) match {
      case (iv1: InferenceVariable, iv2: InferenceVariable) => processBoth(iv1, iv2, assignments, context)
      case (iv1: InferenceVariable, t2) => processIv1(iv1, t2, assignments, context)
      case (t1, iv2: InferenceVariable) => processIv2(t1, iv2, assignments, context)

      case (t1: TupleType, t2: TupleType) => Matchers.matchTuple(t1, t2, assignments, rec, expectedSubtype)

      case (f1: FunctionType, f2: FunctionType) => rec(assignments, f2.input, f1.input).flatMap(rec(_, f1.output, f2.output))

      case (l1: ListType, l2: ListType) => rec(assignments, l1.element, l2.element)

      case (m1: MapType, m2: MapType) => rec(assignments, m1.key, m2.key).flatMap(rec(_, m1.value, m2.value))

      case (s1: ShapeType, s2: ShapeType) =>
        s2.correlate(s1).foldLeft(Compilation.succeed(assignments)) {
          case (compilation, (p2, Some(p1))) => compilation.flatMap(rec(_, p1.tpe, p2.tpe))
          case (_, (_, None)) => expectedSubtype
        }
      case (d1: DeclaredType, s2: ShapeType) => rec(assignments, d1.asShapeType, s2)

      case (_: IntersectionType, _) => unsupported
      case (_, _: IntersectionType) => unsupported
      case (_: SumType, _) => unsupported
      case (_, _: SumType) => unsupported

      // Decides subtype relationships such as `Nothing <:< [iv]`, where the Nothing cannot be matched directly to the
      // type on the right-hand side. This would ordinarily be caught by the "fully instantiated" type check above when
      // both t1 and t2 are fully instantiated, but not when t1 or t2 contain inference variables.
      case (BasicType.Nothing, _) => assignments.compiled
      case (_, BasicType.Any) => assignments.compiled

      case _ => expectedSubtype
    }
  }

}
