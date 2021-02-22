package lore.compiler.phases.typing

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.phases.typing.Inference.{InvalidLowerBound, InvalidUpperBound, NonassignableType, variables}
import lore.compiler.types._

object InferenceAssignments {

  type Assignments = Map[InferenceVariable, Bounds]


  case class Bounds(variable: InferenceVariable, lower: Option[Type], upper: Option[Type]) {
    val lowerOrNothing: Type = lower.getOrElse(BasicType.Nothing)
    val upperOrAny: Type = upper.getOrElse(BasicType.Any)
  }

  // In Lore: type BoundType = #lower | #upper
  sealed trait BoundType
  object BoundType {
    case object Lower extends BoundType
    case object Upper extends BoundType
  }

  def isDefined(assignments: Assignments, inferenceVariable: InferenceVariable): Boolean = assignments.contains(inferenceVariable)

  /**
    * Whether the given bounds are defined for the given inference variable.
    */
  def areBoundsDefined(assignments: Assignments, inferenceVariable: InferenceVariable, boundTypes: Vector[BoundType]): Boolean = {
    assignments.get(inferenceVariable).exists { bounds =>
      (!boundTypes.contains(BoundType.Lower) || bounds.lower.isDefined) && (!boundTypes.contains(BoundType.Upper) || bounds.upper.isDefined)
    }
  }

  /**
    * Instantiate all inference variables in the given type with their corresponding lower or upper bounds.
    */
  def instantiate(assignments: Assignments, tpe: Type, boundType: BoundType): Type = {
    // TODO: Wouldn't this inference constellation be a good test for Lore inference? Lore should be able to infer
    //       Type => Type for (t => instantiate(mode, t)) as long as that's the only function instance of instantiate
    //       with an arity of 2.
    val rec = t => instantiate(assignments, t, boundType)

    tpe match {
      case iv: InferenceVariable =>
        val bounds = assignments.getOrElse(iv, throw CompilationException(s"The inference variable $iv should be defined."))
        boundType match {
          case BoundType.Lower => bounds.lowerOrNothing
          case BoundType.Upper => bounds.upperOrAny
        }
      case tv: TypeVariable =>
        if (Inference.isFullyInferred(tv)) tv
        else ??? // TODO: How can we instantiate the type variable without destroying its reference equality? Maybe a type variable requires a UUID instead?
      case SumType(types) => SumType.construct(types.map(rec))
      case IntersectionType(types) => IntersectionType.construct(types.map(rec))
      case ProductType(elements) => ProductType(elements.map(rec))
      case FunctionType(input, output) => FunctionType(rec(input), rec(output))
      case ListType(element) => ListType(rec(element))
      case MapType(key, value) => MapType(rec(key), rec(value))
      case shapeType: ShapeType => shapeType.mapPropertyTypes(rec)
      case tpe => tpe
    }
  }

  def effectiveBounds(assignments: Assignments, inferenceVariable: InferenceVariable): Bounds = {
    assignments.getOrElse(inferenceVariable, Bounds(inferenceVariable, None, None))
  }

  def narrowBounds(assignments: Assignments, inferenceVariable: InferenceVariable, lowerBound: Type, upperBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    narrowBound(assignments, inferenceVariable, lowerBound, BoundType.Lower, context).flatMap(
      narrowBound(_, inferenceVariable, upperBound, BoundType.Upper, context)
    )
  }

  /**
    * Attempts to narrow the lower/upper bound of the inference variable to the given new lower/upper bound. If the
    * variable already has a lower/upper bound, the new lower/upper bound must supertype/subtype the existing bound.
    */
  def narrowBound(assignments: Assignments, inferenceVariable: InferenceVariable, bound: Type, boundType: BoundType, context: TypingJudgment): Compilation[Assignments] = boundType match {
    case BoundType.Lower => narrowLowerBound(assignments, inferenceVariable, bound, context)
    case BoundType.Upper => narrowUpperBound(assignments, inferenceVariable, bound, context)
  }

  /**
    * Attempts to narrow the lower bound of the inference variable to the given new lower bound. If the variable
    * already has a lower bound, the new lower bound must supertype the existing bound.
    */
  def narrowLowerBound(assignments: Assignments, inferenceVariable: InferenceVariable, lowerBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    val bounds = effectiveBounds(assignments, inferenceVariable)

    // TODO: Also ensure that the new lower bound is a subtype of the existing upper bound. Write an error message for this case.
    if (bounds.lower.forall(Subtyping.isSubtype(_, lowerBound))) {
      Compilation.succeed(assignments.updated(inferenceVariable, Bounds(inferenceVariable, Some(lowerBound), bounds.upper)))
    } else {
      Compilation.fail(InvalidLowerBound(inferenceVariable, lowerBound, bounds.lowerOrNothing, context))
    }
  }

  /**
    * Attempts to narrow the upper bound of the inference variable to the given new upper bound. If the variable
    * already has an upper bound, the new upper bound must subtype the existing bound.
    */
  def narrowUpperBound(assignments: Assignments, inferenceVariable: InferenceVariable, upperBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    val bounds = effectiveBounds(assignments, inferenceVariable)

    // TODO: Also ensure that the new upper bound is a supertype of the existing lower bound. Write an error message for this case.
    if (bounds.upper.forall(Subtyping.isSubtype(upperBound, _))) {
      Compilation.succeed(assignments.updated(inferenceVariable, Bounds(inferenceVariable, bounds.lower, Some(upperBound))))
    } else {
      Compilation.fail(InvalidUpperBound(inferenceVariable, upperBound, bounds.upperOrAny, context))
    }
  }

  /**
    * Override the bounds of the given inference variable without checking the previous bounds. This function is useful
    * when processing "dependent" typings that might change the bounds of an inference variable altogether based on
    * some other changing inference variables.
    */
  def overrideBounds(assignments: Assignments, inferenceVariable: InferenceVariable, lowerBound: Type, upperBound: Type): Assignments = {
    assignments.updated(inferenceVariable, Bounds(inferenceVariable, Some(lowerBound), Some(upperBound)))
  }

  /**
    * @see [[ensureBoundSupertypes]], [[ensureBoundSubtypes]]
    */
  def ensureBound(assignments: Assignments, inferenceVariable: InferenceVariable, bound: Type, boundType: BoundType, context: TypingJudgment): Compilation[Assignments] = boundType match {
    case BoundType.Lower => ensureBoundSupertypes(assignments, inferenceVariable, bound, context)
    case BoundType.Upper => ensureBoundSubtypes(assignments, inferenceVariable, bound, context)
  }

  /**
    * Ensures that the inference variable's lower bound is a supertype of the given lower bound. If this is not the
    * case already, the function will attempt to narrow the inference variable's lower bound.
    *
    * In practical terms, the function thus assures that a given supertyping relationship holds, either by validating
    * it directly or by changing the bounds to "make it fit".
    */
  def ensureBoundSupertypes(assignments: Assignments, inferenceVariable: InferenceVariable, lowerBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    val bounds = effectiveBounds(assignments, inferenceVariable)

    if (Subtyping.isSubtype(lowerBound, bounds.lowerOrNothing)) {
      Compilation.succeed(assignments)
    } else {
      narrowLowerBound(assignments, inferenceVariable, lowerBound, context)
    }
  }

  /**
    * Ensures that the inference variable's upper bound is a subtype of the given upper bound. If this is not the
    * case already, the function will attempt to narrow the inference variable's upper bound.
    *
    * In practical terms, the function thus assures that a given subtyping relationship holds, either by validating
    * it directly or by changing the bounds to "make it fit".
    */
  def ensureBoundSubtypes(assignments: Assignments, inferenceVariable: InferenceVariable, upperBound: Type, context: TypingJudgment): Compilation[Assignments] = {
    val bounds = effectiveBounds(assignments, inferenceVariable)

    if (Subtyping.isSubtype(bounds.upperOrAny, upperBound)) {
      Compilation.succeed(assignments)
    } else {
      narrowUpperBound(assignments, inferenceVariable, upperBound, context)
    }
  }

  /**
    * Matches all types from `source` with their corresponding inference variables in `target`, invoking the
    * `process` function for each such pair.
    */
  def correlate(
    process: (Assignments, InferenceVariable, Type, BoundType, TypingJudgment) => Compilation[Assignments]
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

    // TODO: Can we even live with unsupported assignments here or do we have to bite the bullet? Sum and
    //       intersection types need to also be part of type inference beyond the most basic aspects...
    def unsupported: Nothing = {
      throw CompilationException(s"Inference variable correlation of intersection and sum types is not yet supported." +
        s" Given types: $actualSource and $target.")
    }

    // TODO: Move the error here?
    def nonassignableType: Compilation[Assignments] = Compilation.fail(NonassignableType(source, target, context))

    // TODO: We can still merge this implementation with the one from TypeVariableAllocation, right? (This depends on
    //       how we want to handle sum/intersection types, though.)
    val rec = (newAssignments: Assignments, newSource: Type, newTarget: Type) => correlate(process)(newAssignments, newSource, newTarget, boundType, context)
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
        } else nonassignableType

      case (f1: FunctionType, f2: FunctionType) => rec(assignments, f1.input, f2.input).flatMap(rec(_, f1.output, f2.output))

      case (l1: ListType, l2: ListType) => rec(assignments, l1.element, l2.element)

      case (m1: MapType, m2: MapType) => rec(assignments, m1.key, m2.key).flatMap(rec(_, m1.value, m2.value))

      case (s1: ShapeType, s2: ShapeType) =>
        s2.correlate(s1).foldLeft(Compilation.succeed(assignments)) {
          case (compilation, (p2, Some(p1))) => compilation.flatMap(rec(_, p1.tpe, p2.tpe))
          case (_, (_, None)) => nonassignableType
        }
      case (s1: StructType, s2: ShapeType) => rec(assignments, s1.asShapeType, s2)

      // TODO: Rewrite the following comment:
      // Allocating types to intersection types and sum types is quite complex, since the allocation mechanism
      // suddenly comes upon more than one possible allocation. Take, for example, a sum type A | B, to which we
      // try to assign a type C. Should A or B become C? Surely not both A and B can be C (unless the sum type
      // is trivial). And even if we have a structurally similar type C | D, should A = C and B = D or A = D and
      // B = C? There are multiple possibilities.
      case (_: IntersectionType, _) => unsupported
      case (_, _: IntersectionType) => unsupported
      case (_: SumType, _) => unsupported
      case (_, _: SumType) => unsupported

      // In all other cases, there is no need to assign anything.
      case _ => Compilation.succeed(assignments)
    }
  }

  /**
    * Correlates `source` to `target` if all inference variables in `source` are defined for all the given bound types.
    */
  def correlateIfDefined(
    process: (Assignments, InferenceVariable, Type, BoundType, TypingJudgment) => Compilation[Assignments]
  )(assignments: Assignments, source: Type, target: Type, boundTypes: Vector[BoundType], context: TypingJudgment): Compilation[Assignments] = {
    if (Inference.variables(source).forall(areBoundsDefined(assignments, _, boundTypes))) {
      boundTypes.foldLeft(Compilation.succeed(assignments)) {
        case (compilation, boundType) => compilation.flatMap(correlate(process)(_, source, target, boundType, context))
      }
    } else Compilation.succeed(assignments)
  }

}
