package lore.compiler.inference

import com.typesafe.scalalogging.Logger
import lore.compiler.core.{Compilation, Errors, Result}
import lore.compiler.feedback.Feedback
import lore.compiler.inference.InferenceBounds.BoundType
import lore.compiler.semantics.Registry
import lore.compiler.types._
import lore.compiler.utils.Timer.timed

object Inference {

  type Assignments = Map[InferenceVariable, InferenceBounds]

  def infer(judgments: Vector[TypingJudgment])(implicit registry: Registry): Compilation[Assignments] = {
    // Note that the order of judgments is important for reproducibility: we should always process the judgments in
    // their order of declaration. In addition, this will give the algorithm the best chance at resolving type
    // inference in one go, as the flow of typing most often follows the natural judgment order.
    SimpleResolution.infer(InferenceBounds.prefill(Map.empty, judgments), judgments)
  }

  /**
    * This logger is used to log inference minutiae.
    */
  val logger: Logger = Logger("lore.compiler.inference")
  val loggerBlank: Logger = Logger("lore.compiler.inference.blank")

  def inferVerbose(judgments: Vector[TypingJudgment], label: String)(implicit registry: Registry): Compilation[Assignments] = {
    if (judgments.isEmpty) {
      return Compilation.succeed(Map.empty)
    }

    logger.debug(s"Typing judgments for $label:\n${judgments.mkString("\n")}")

    val result = timed(s"Inference for $label", log = s => logger.debug(s)) {
      infer(judgments) match {
        case result@Result(_, _) =>
          logger.debug(s"Inference for $label was successful with the following inferred types:\n${result.value.stringified}\n")
          result
        case errors@Errors(_, _) =>
          logger.debug(s"Inference for $label failed with the following feedback:")
          logger.whenDebugEnabled {
            Feedback.logAll(errors.feedback)
          }
          errors
      }
    }

    loggerBlank.debug("")
    result
  }

  /**
    * This class is intended as an API for other components such as the [[lore.compiler.phases.transformation.TypeRehydrationVisitor]].
    */
  implicit class AssignmentsExtension(assignments: Assignments) {
    def instantiate(tpe: Type): Type = Inference.instantiate(assignments, tpe, _.candidateType)
    def stringified: String = assignments.values.toVector.sortBy(_.variable.name).mkString("\n")
  }

  /**
    * Collects all inference variables contained in the given type.
    */
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
    * Instantiates all defined inference variables in `tpe` to a type given by `get`. Undefined inference variables
    * lead to a compilation exception, as all inference variables occurring in any of the judgments should at least
    * have Nothing/Any as their bounds.
    */
  def instantiate(assignments: Assignments, tpe: Type, get: InferenceBounds => Type): Type = {
    // `instantiate` may be called with simple types quite often. We want to avoid reconstructing types (with  all the
    // required allocations) in such cases.
    if (isFullyInstantiated(tpe)) {
      return tpe
    }

    val rec = (t: Type) => instantiate(assignments, t, get)
    tpe match {
      case iv: InferenceVariable => get(InferenceVariable.bounds(iv, assignments))
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
