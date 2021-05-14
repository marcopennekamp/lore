package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.{Compilation, CompilationException, Error}
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiateByBound, isFullyInstantiated}
import lore.compiler.phases.transformation.inference.InferenceBounds.{BoundType, ensureBound}
import lore.compiler.phases.transformation.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.types._

object SubtypesJudgmentResolver extends JudgmentResolver[TypingJudgment.Subtypes] {

  override def forwards(
    judgment: TypingJudgment.Subtypes,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    println(s"Resolve $judgment")
    val t2 = instantiateByBound(assignments, judgment.t2, BoundType.Upper)
    val result = ensureSubtypes(assignments, judgment.t1, t2, judgment)
    println(result)
    println()
    result
  }

  override def backwards(
    judgment: TypingJudgment.Subtypes,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    ensureSubtypes(assignments, instantiateByBound(assignments, judgment.t1, BoundType.Lower), judgment.t2, judgment)
  }

  case class ExpectedSubtype(t1: Type, t2: Type, context: TypingJudgment) extends Error(context) {
    override def message: String = s"$t1 is not a subtype of $t2."
  }

  /**
    * Ensures that `t1` is a subtype of `t2`:
    *
    *   - If `t1` and `t2` are both fully instantiated, this resolution defers to a simple subtyping check.
    *   - Otherwise, if `t1` or `t2` contain inference variables, matching types are assigned to the inference
    *     variables. The resulting assignments are built such that `instantiate(t1) <= instantiate(t2)`.
    */
  def ensureSubtypes(assignments: Assignments, t1: Type, t2: Type, context: TypingJudgment): Compilation[Assignments] = {
    if (isFullyInstantiated(t1) && isFullyInstantiated(t2)) {
      return if (t1 <= t2) Compilation.succeed(assignments) else Compilation.fail(ExpectedSubtype(t1, t2, context))
    }

    def unsupported: Nothing = {
      throw CompilationException(s"Inference subtype matching of intersection and sum types is not yet supported." +
        s" Given types: $t1 and $t2.")
    }

    def expectedSubtype: Compilation[Assignments] = Compilation.fail(ExpectedSubtype(t1, t2, context))

    val rec = (newAssignments: Assignments, u1: Type, u2: Type) => ensureSubtypes(newAssignments, u1, u2, context)
    (t1, t2) match {
      case (t1, iv2: InferenceVariable) => ensureBound(assignments, iv2, t1, BoundType.Lower, context)
      case (iv1: InferenceVariable, t2) => ensureBound(assignments, iv1, t2, BoundType.Upper, context)
      case (_: InferenceVariable, _: InferenceVariable) => ??? // TODO: Either resolve this case or throw a proper error.

      case (tv1: TypeVariable, tv2: TypeVariable) =>
        // TODO: Do we need to assign lower and upper bounds of type variables for inference????
        ???

      case (p1: ProductType, p2: ProductType) =>
        if (p1.elements.size == p2.elements.size) {
          p1.elements.zip(p2.elements).foldLeft(Compilation.succeed(assignments)) {
            case (compilation, (e1, e2)) => compilation.flatMap(rec(_, e1, e2))
          }
        } else expectedSubtype

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
