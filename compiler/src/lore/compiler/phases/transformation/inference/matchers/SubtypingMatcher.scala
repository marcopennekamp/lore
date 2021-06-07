package lore.compiler.phases.transformation.inference.matchers

import lore.compiler.core.{Compilation, CompilationException, Error}
import lore.compiler.phases.transformation.inference.Inference.{Assignments, isFullyInstantiated}
import lore.compiler.phases.transformation.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.types._

object SubtypingMatcher {

  case class ExpectedSubtype(t1: Type, t2: Type, context: TypingJudgment) extends Error(context) {
    override def message: String = s"$t1 should be a subtype of $t2, but is not."
  }

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
  )(t1: Type, t2: Type, assignments: Assignments, context: TypingJudgment): Compilation[Assignments] = {
    if (isFullyInstantiated(t1) && isFullyInstantiated(t2)) {
      return if (t1 <= t2) Compilation.succeed(assignments) else Compilation.fail(ExpectedSubtype(t1, t2, context))
    }

    def unsupported: Nothing = {
      throw CompilationException(s"Inference subtype matching of intersection and sum types is not yet supported." +
        s" Given types: $t1 and $t2.")
    }

    def expectedSubtype = Compilation.fail(ExpectedSubtype(t1, t2, context))

    val rec = (newAssignments: Assignments, u1: Type, u2: Type) => matchSubtype(processIv1, processIv2)(u1, u2, newAssignments, context)
    (t1, t2) match {
      case (_: InferenceVariable, _: InferenceVariable) => ??? // TODO: Either resolve this case or throw a proper error.
      case (iv1: InferenceVariable, t2) => processIv1(iv1, t2, assignments, context)
      case (t1, iv2: InferenceVariable) => processIv2(t1, iv2, assignments, context)

      case (p1: ProductType, p2: ProductType) => Matchers.matchTuple(p1, p2, assignments, rec, expectedSubtype)

      case (f1: FunctionType, f2: FunctionType) => rec(assignments, f1.input, f2.input).flatMap(rec(_, f1.output, f2.output))

      case (l1: ListType, l2: ListType) => rec(assignments, l1.element, l2.element)

      case (m1: MapType, m2: MapType) => rec(assignments, m1.key, m2.key).flatMap(rec(_, m1.value, m2.value))

      case (s1: ShapeType, s2: ShapeType) =>
        s2.correlate(s1).foldLeft(Compilation.succeed(assignments)) {
          case (compilation, (p2, Some(p1))) => compilation.flatMap(rec(_, p1.tpe, p2.tpe))
          case (_, (_, None)) => expectedSubtype
        }
      case (d1: DeclaredType, s2: ShapeType) => rec(assignments, d1.asShapeType, s2)

      // TODO: Can we even live with unsupported assignments here or do we have to bite the bullet? Sum and
      //       intersection types need to also be part of type inference beyond the most basic aspects...
      case (_: IntersectionType, _) => unsupported
      case (_, _: IntersectionType) => unsupported
      case (_: SumType, _) => unsupported
      case (_, _: SumType) => unsupported

      case _ => expectedSubtype
    }
  }

}