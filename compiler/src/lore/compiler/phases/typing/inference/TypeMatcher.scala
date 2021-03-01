package lore.compiler.phases.typing.inference

import lore.compiler.core.{Compilation, CompilationException, Error}
import Inference.{Assignments, instantiate, variables}
import lore.compiler.phases.typing.inference.InferenceBounds.BoundType
import lore.compiler.phases.typing.inference.InferenceVariable.{isDefined, isDefinedAt}
import lore.compiler.types._

object TypeMatcher {

  case class UndefinedInferenceVariables(source: Type, target: Type, context: TypingJudgment) extends Error(context) {
    override def message: String = s"Not all inference variables in $source are defined and thus cannot be used to " +
      s"narrow the bounds of all inference variables in $target."
  }

  /**
    * If all inference variables in `source` are defined, the function matches all inference variables in `target`
    * using [[InferenceBounds.narrowBound]] for both bound types.
    *
    * If some inference variables in `source` are not yet defined, the operation fails and results in a compilation
    * error. This error will usually be recovered from during Equals judgment resolution.
    */
  def narrowBounds(assignments: Assignments, source: Type, target: Type, context: TypingJudgment): Compilation[Assignments] = {
    if (Inference.variables(source).forall(isDefined(assignments, _))) {
      matchAll(InferenceBounds.narrowBound)(assignments, source, target, BoundType.Lower, context).flatMap(
        matchAll(InferenceBounds.narrowBound)(_, source, target, BoundType.Upper, context)
      )
    } else Compilation.fail(UndefinedInferenceVariables(source, target, context))
  }

  /**
    * If all inference variables in `source` are defined at the given bound type, the function ensures the bound of all
    * inference variables in `target` using [[InferenceBounds.ensureBound]].
    */
  def ensureBoundsIfDefined(assignments: Assignments, source: Type, target: Type, boundType: BoundType, context: TypingJudgment): Compilation[Assignments] = {
    if (Inference.variables(source).forall(isDefinedAt(assignments, _, boundType))) {
      matchAll(InferenceBounds.ensureBound)(assignments, source, target, boundType, context)
    } else Compilation.succeed(assignments)
  }

  case class IncompatibleMatch(source: Type, target: Type, context: TypingJudgment) extends Error(context) {
    override def message: String = s"The type $source cannot be matched to the type $target. Inference judgment in question: $context."
  }

  /**
    * Instantiates `source` (using the given `boundType`) and matches all types therein to their corresponding
    * inference variables in `target`, invoking `process` for each such pair. Incompatible matches lead to a
    * compilation error, so the function guarantees that `process` will be called for each target inference variable
    * at least once.
    */
  def matchAll(
    process: (Assignments, InferenceVariable, Type, BoundType, TypingJudgment) => Compilation[Assignments],
  )(assignments: Assignments, source: Type, target: Type, boundType: BoundType, context: TypingJudgment): Compilation[Assignments] = {
    // If the target type contains no inference variables, there is no way we could process any, and thus the
    // operation can be skipped. This check is currently important for correct compiler operation, as we only want to
    // raise an "unsupported correlation" error in cases where the target type even contains inference variables.
    if (variables(target).isEmpty) {
      return Compilation.succeed(assignments)
    }

    val actualSource = instantiate(assignments, source, boundType)
    if (variables(actualSource).nonEmpty) {
      throw CompilationException(s"The source $actualSource should have been correlated with target $target, but the source still contains uninstantiated inference variables.")
    }

    def unsupported: Nothing = {
      throw CompilationException(s"Inference variable correlation of intersection and sum types is not yet supported." +
        s" Given types: $actualSource and $target.")
    }

    def incompatibleMatch: Compilation[Assignments] = Compilation.fail(IncompatibleMatch(actualSource, target, context))

    val rec = (newAssignments: Assignments, newSource: Type, newTarget: Type) => matchAll(process)(newAssignments, newSource, newTarget, boundType, context)
    (actualSource, target) match {
      case (t1, iv2: InferenceVariable) => process(assignments, iv2, t1, boundType, context)

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

}
