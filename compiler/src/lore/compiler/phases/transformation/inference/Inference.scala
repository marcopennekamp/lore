package lore.compiler.phases.transformation.inference

import lore.compiler.core.Compilation
import lore.compiler.phases.transformation.inference.InferenceBounds.BoundType
import lore.compiler.semantics.Registry
import lore.compiler.types._

object Inference {

  type Assignments = Map[InferenceVariable, InferenceBounds]

  def infer(judgments: Vector[TypingJudgment])(implicit registry: Registry): Compilation[Assignments] = {
    // Note that the order of judgments is important for reproducibility: we should always process the judgments in
    // their order of declaration. In addition, this will give the algorithm the best chance at resolving type
    // inference in one go, as the flow of typing most often follows the natural judgment order.
    SimpleResolution.infer(Map.empty, judgments)
  }

  /**
    * This class is intended as an API for other components such as the [[lore.compiler.phases.transformation.TypeRehydrationVisitor]].
    */
  implicit class AssignmentsExtension(assignments: Assignments) {
    def instantiate(tpe: Type): Type = Inference.instantiate(assignments, tpe, _.candidateType)
    def stringified: String = assignments.values.toVector.sortBy(_.variable.name).mkString("\n")
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
  def isFullyInstantiated(tpe: Type): Boolean = tpe match {
    case _: InferenceVariable => false
    case tv: TypeVariable => isFullyInstantiated(tv.lowerBound) && isFullyInstantiated(tv.upperBound)
    case SumType(types) => types.forall(isFullyInstantiated)
    case IntersectionType(types) => types.forall(isFullyInstantiated)
    case ProductType(elements) => elements.forall(isFullyInstantiated)
    case FunctionType(input, output) => isFullyInstantiated(input) && isFullyInstantiated(output)
    case ListType(element) => isFullyInstantiated(element)
    case MapType(key, value) => isFullyInstantiated(key) && isFullyInstantiated(value)
    case ShapeType(properties) => properties.values.map(_.tpe).forall(isFullyInstantiated)
    case _: NamedType => true
  }

  /**
    * Instantiates all defined inference variables in `tpe` to the respective bound.
    */
  def instantiateByBound(assignments: Assignments, tpe: Type, boundType: BoundType): Type = {
    instantiate(assignments, tpe, _.get(boundType))
  }

  /**
    * Instantiates all defined inference variables in `tpe` to a type given by `get`. Undefined inference variables are
    * left as-is.
    */
  def instantiate(assignments: Assignments, tpe: Type, get: InferenceBounds => Type): Type = instantiate(assignments, tpe, get, identity)

  /**
    * Instantiates all defined inference variables in `tpe` to a type given by `get`. Undefined inference variables are
    * instantiated to a type given by `undefined`.
    */
  def instantiate(assignments: Assignments, tpe: Type, get: InferenceBounds => Type, undefined: InferenceVariable => Type): Type = {
    // `instantiate` may be called with simple types quite often. We want to avoid reconstructing types (with  all the
    // required allocations) in such cases.
    if (isFullyInstantiated(tpe)) {
      return tpe
    }

    val rec = (t: Type) => instantiate(assignments, t, get, undefined)
    tpe match {
      case iv: InferenceVariable => assignments.get(iv).map(get).getOrElse(undefined(iv))
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
