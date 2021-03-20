package lore.compiler.phases.typing.inference

import lore.compiler.core.Compilation
import lore.compiler.phases.typing.inference.InferenceBounds.BoundType
import lore.compiler.semantics.Registry
import lore.compiler.types._

object Inference {

  type Assignments = Map[InferenceVariable, InferenceBounds]

  def infer(judgments: Vector[TypingJudgment])(implicit registry: Registry): Compilation[Assignments] = {
    // TODO: Judgments are only reversed to test the robustness of the inference algorithm! Remove later!!!
    // Note that the order of judgments is important for reproducibility: we should always process the judgments in
    // their order of declaration. In addition, this will give the algorithm the best chance at resolving type
    // inference in one go, as the flow of typing most often follows the natural judgment order.
    BulkResolution.infer(Map.empty, judgments.reverse).flatMap { assignments =>
      // Once all inference variables have been instantiated, make another pass over all judgments to check equality
      // and subtyping constraints.
      judgments.map(JudgmentChecker.check(_, assignments)).simultaneous.map(_ => assignments)
    }
  }

  /**
    * This class is intended as an API for other components such as the [[lore.compiler.phases.typing.TypeRehydrationVisitor]].
    */
  implicit class AssignmentsExtension(assignments: Assignments) {
    def instantiate(tpe: Type): Type = Inference.instantiate(assignments, tpe, _.candidateType)
  }

  def variables(tpe: Type): Set[InferenceVariable] = tpe match {
    case iv: InferenceVariable => Set(iv)
    case tv: TypeVariable => variables(tv.lowerBound) ++ variables(tv.upperBound)
    case SumType(types) => types.flatMap(variables)
    case IntersectionType(types) => types.flatMap(variables)
    case ProductType(elements) => elements.flatMap(variables).toSet
    case FunctionType(input, output) => variables(input) ++ variables(output)
    case ListType(element) => variables(element)
    case MapType(key, value) => variables(key) ++ variables(value)
    case ShapeType(properties) => properties.values.map(_.tpe).flatMap(variables).toSet
    case _: NamedType => Set.empty
  }

  /**
    * Whether the given type contains no inference variables at all, which means that it can bypass type inference.
    */
  def isFullyInferred(tpe: Type): Boolean = tpe match {
    case _: InferenceVariable => false
    case tv: TypeVariable => isFullyInferred(tv.lowerBound) && isFullyInferred(tv.upperBound)
    case SumType(types) => types.forall(isFullyInferred)
    case IntersectionType(types) => types.forall(isFullyInferred)
    case ProductType(elements) => elements.forall(isFullyInferred)
    case FunctionType(input, output) => isFullyInferred(input) && isFullyInferred(output)
    case ListType(element) => isFullyInferred(element)
    case MapType(key, value) => isFullyInferred(key) && isFullyInferred(value)
    case ShapeType(properties) => properties.values.map(_.tpe).forall(isFullyInferred)
    case _: NamedType => true
  }

  /**
    * Instantiates all defined inference variables in `tpe` to the respective bound.
    */
  def instantiateByBound(assignments: Assignments, tpe: Type, boundType: BoundType): Type = {
    instantiate(
      assignments,
      tpe,
      bounds => boundType match {
        case BoundType.Lower => bounds.lowerOrNothing
        case BoundType.Upper => bounds.upperOrAny
      }
    )
  }

  /**
    * Instantiates all defined inference variables in `tpe` to a type given by `get`.
    */
  def instantiate(assignments: Assignments, tpe: Type, get: InferenceBounds => Type): Type = {
    // `instantiate` may be called with fully inferred types quite often. We want to avoid reconstructing types (with
    // all the required allocations) in such cases.
    if (isFullyInferred(tpe)) {
      return tpe
    }

    // TODO: Wouldn't this inference constellation be a good test for Lore inference? Lore should be able to infer
    //       Type => Type for (t => instantiate(mode, t)) as long as that's the only function instance of instantiate
    //       with an arity of 2.
    val rec = t => instantiate(assignments, t, get)

    tpe match {
      case iv: InferenceVariable => assignments.get(iv).map(get).getOrElse(iv)
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

}
