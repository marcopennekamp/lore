package lore.compiler.phases.transformation.inference

import lore.compiler.core.{Compilation, CompilationException, Error}
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiate, instantiateByBound, isFullyInferred}
import lore.compiler.phases.transformation.inference.InferenceBounds.{BoundType, narrowBound, narrowBounds, overrideBounds}
import lore.compiler.phases.transformation.inference.InferenceVariable.{effectiveBounds, isDefined}
import lore.compiler.phases.transformation.inference.TypeMatcher.{IncompatibleMatch, matchAll}
import lore.compiler.types._

object Unification {

  case class CannotUnifyIncompatibleTypes(t1: Type, t2: Type, context: TypingJudgment) extends Error(context) {
    override def message: String = s"Cannot unify types $t1 and $t2. The types don't match."
  }

  /**
    * Unifies `t1` with `t2`, ensuring that all inference variables in both `t1` and `t2` are defined such that
    * `instantiate(t1) == instantiate(t2)`.
    */
  def unify(assignments: Assignments, t1: Type, t2: Type, context: TypingJudgment): Compilation[Assignments] = {
    def unsupported: Nothing = {
      throw CompilationException(s"Inference unification of intersection and sum types is not yet supported." +
        s" Given types: $t1 and $t2.")
    }

    // TODO: Use error customized for unification.
    def incompatibleMatch: Compilation[Assignments] = Compilation.fail(IncompatibleMatch(t1, t2, context))

    val rec = (assignments2: Assignments, u1: Type, u2: Type) => unify(assignments2, u1, u2, context)
    (t1, t2) match {
      case (iv1: InferenceVariable, iv2: InferenceVariable) => unifyInferenceVariables(assignments, iv1, iv2, context)
      case (iv1: InferenceVariable, t2) => unifyInferenceVariableWithType(assignments, iv1, t2, context)
      case (t1, iv2: InferenceVariable) => unifyInferenceVariableWithType(assignments, iv2, t1, context)

      case (tv1: TypeVariable, tv2: TypeVariable) =>
        // TODO: Do we need to assign lower and upper bounds of type variables for inference????
        ???

      case (p1: ProductType, p2: ProductType) =>
        if (p1.elements.size == p2.elements.size) {
          p1.elements.zip(p2.elements).foldLeft(Compilation.succeed(assignments)) {
            case (compilation, (e1, e2)) => compilation.flatMap(rec(_, e1, e2))
          }
        } else incompatibleMatch

      case (f1: FunctionType, f2: FunctionType) => rec(assignments, f1.input, f2.input).flatMap(rec(_, f1.output, f2.output))

      case (l1: ListType, l2: ListType) => rec(assignments, l1.element, l2.element)

      case (m1: MapType, m2: MapType) => rec(assignments, m1.key, m2.key).flatMap(rec(_, m1.value, m2.value))

      case (s1: ShapeType, s2: ShapeType) =>
        s2.correlate(s1).foldLeft(Compilation.succeed(assignments)) {
          case (compilation, (p2, Some(p1))) => compilation.flatMap(rec(_, p1.tpe, p2.tpe))
          case (_, (_, None)) => incompatibleMatch
        }
      case (s1: StructType, s2: ShapeType) => rec(assignments, s1.asShapeType, s2)

      // TODO: Can we even live with unsupported assignments here or do we have to bite the bullet? Sum and
      //       intersection types need to also be part of type inference beyond the most basic aspects...
      case (_: IntersectionType, _) => unsupported
      case (_, _: IntersectionType) => unsupported
      case (_: SumType, _) => unsupported
      case (_, _: SumType) => unsupported

      case _ => incompatibleMatch
    }
  }

  case class CannotUnifyLowerBounds(bounds1: InferenceBounds, bounds2: InferenceBounds, context: TypingJudgment) extends Error(context) {
    override def message: String = s"Type error: Cannot unify lower bounds of $bounds1 and $bounds2." // TODO: Create a more user-friendly error.
  }

  case class CannotUnifyUpperBounds(bounds1: InferenceBounds, bounds2: InferenceBounds, context: TypingJudgment) extends Error(context) {
    override def message: String = s"Type error: Cannot unify upper bounds of $bounds1 and $bounds2." // TODO: Create a more user-friendly error.
  }

  case class ConflictingBounds(bounds1: InferenceBounds, bounds2: InferenceBounds, context: TypingJudgment) extends Error(context) {
    override def message: String = s"Type error: Cannot unify bounds $bounds1 and $bounds2 because the lower and upper bounds conflict." // TODO: Create a more user-friendly error.
  }

  /**
    * Unify the bounds of the two inference variables such that they have the same bounds which must also be a narrowed
    * version of their prior bounds.
    */
  private def unifyInferenceVariables(assignments: Assignments, iv1: InferenceVariable, iv2: InferenceVariable, context: TypingJudgment): Compilation[Assignments] = {
    val bounds1 = effectiveBounds(assignments, iv1)
    val bounds2 = effectiveBounds(assignments, iv2)

    val lower = Type.maxOrEqual(bounds1.lowerOrNothing, bounds2.lowerOrNothing) match {
      case Some(t) => t
      case None => return Compilation.fail(CannotUnifyLowerBounds(bounds1, bounds2, context))
    }

    val upper = Type.minOrEqual(bounds1.upperOrAny, bounds2.upperOrAny) match {
      case Some(t) => t
      case None => return Compilation.fail(CannotUnifyUpperBounds(bounds1, bounds2, context))
    }

    if (!(lower <= upper)) {
      return Compilation.fail(ConflictingBounds(bounds1, bounds2, context))
    }

    val assignments2 = overrideBounds(assignments, iv1, lower, upper)
    val assignments3 = overrideBounds(assignments2, iv2, lower, upper)

    Compilation.succeed(assignments3)
  }

  /**
    * Unify the given inference variable with the given type. This might not only assign new bounds to the inference
    * variable, but also instantiate the inference variable and assign bounds to any variables contained in `tpe`.
    */
  private def unifyInferenceVariableWithType(assignments: Assignments, iv: InferenceVariable, tpe: Type, context: TypingJudgment): Compilation[Assignments] = {
    val typeLower = instantiateByBound(assignments, tpe, BoundType.Lower)
    val typeUpper = instantiateByBound(assignments, tpe, BoundType.Upper)
    val typeToVariable = if (isFullyInferred(typeLower) && isFullyInferred(typeUpper)) {
      narrowBounds(assignments, iv, typeLower, typeUpper, context)
    } else Compilation.succeed(assignments)

    typeToVariable.flatMap { assignments2 =>
      if (isDefined(assignments2, iv) && !isFullyInferred(tpe)) {
        val ivLower = instantiateByBound(assignments2, iv, BoundType.Lower)
        val ivUpper = instantiateByBound(assignments2, iv, BoundType.Upper)
        matchAll(narrowBound)(assignments2, ivLower, tpe, BoundType.Lower, context).flatMap(
          matchAll(narrowBound)(_, ivUpper, tpe, BoundType.Upper, context)
        )
      } else Compilation.succeed(assignments2)
    }
  }

}
