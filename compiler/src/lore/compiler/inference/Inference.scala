package lore.compiler.inference

import com.typesafe.scalalogging.Logger
import lore.compiler.feedback.{Feedback, MemoReporter, Reporter}
import lore.compiler.inference.InferenceBounds.BoundType
import lore.compiler.semantics.Registry
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.types._
import lore.compiler.utils.Timer.timed

object Inference {

  type Assignments = Map[InferenceVariable, InferenceBounds]

  /**
    * Infer a set of assignments from the given typing judgments.
    *
    * Inference currently operates conservatively, terminating after an error has been found. A partial assignment map
    * will always be returned.
    */
  def infer(judgments: Vector[TypingJudgment])(implicit registry: Registry, reporter: Reporter): Assignments = {
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

  def inferVerbose(judgments: Vector[TypingJudgment], label: String, parentReporter: Reporter)(implicit registry: Registry): Assignments = {
    if (judgments.isEmpty) {
      return Map.empty
    }

    logger.debug(s"Typing judgments for $label:\n${judgments.mkString("\n")}")

    val result = timed(s"Inference for $label", log = s => logger.debug(s)) {
      MemoReporter.nested(parentReporter) { implicit reporter =>
        val assignments = infer(judgments)

        if (!reporter.hasErrors) {
          logger.debug(s"Inference for $label was successful with the following inferred types:\n${assignments.stringified}\n")
        } else {
          logger.debug(s"Inference for $label failed with the following feedback:")
          logger.whenDebugEnabled {
            Feedback.logAll(reporter.feedback)
          }
        }

        assignments
      }
    }

    loggerBlank.debug("")
    result
  }

  /**
    * This class is intended as an API for other components such as the [[lore.compiler.phases.transformation.TypeRehydrationVisitor]].
    */
  implicit class AssignmentsExtension(assignments: Assignments) {
    def instantiate(tpe: Type): Type = Inference.instantiateCandidateType(assignments, tpe)
    def stringified: String = assignments.values.toVector.sortBy(_.variable.label).mkString("\n")
  }

  /**
    * Collects all inference variables contained in the given type.
    */
  def variables(tpe: Type): Set[InferenceVariable] = tpe match {
    case iv: InferenceVariable => Set(iv)
    case SumType(types) => types.flatMap(variables)
    case IntersectionType(types) => types.flatMap(variables)
    case TupleType(elements) => elements.flatMap(variables).toSet
    case FunctionType(input, output) => variables(input) ++ variables(output)
    case ListType(element) => variables(element)
    case MapType(key, value) => variables(key) ++ variables(value)
    case ShapeType(properties) => properties.values.map(_.tpe).flatMap(variables).toSet
    case dt: DeclaredType if !dt.schema.isConstant => dt.typeArguments.flatMap(variables).toSet
    case _ => Set.empty
  }

  /**
    * Whether the given type contains no inference variables at all, which means that it can bypass type inference.
    */
  def isFullyInstantiated(tpe: Type): Boolean = tpe match {
    case _: InferenceVariable => false
    case SumType(types) => types.forall(isFullyInstantiated)
    case IntersectionType(types) => types.forall(isFullyInstantiated)
    case TupleType(elements) => elements.forall(isFullyInstantiated)
    case FunctionType(input, output) => isFullyInstantiated(input) && isFullyInstantiated(output)
    case ListType(element) => isFullyInstantiated(element)
    case MapType(key, value) => isFullyInstantiated(key) && isFullyInstantiated(value)
    case ShapeType(properties) => properties.values.map(_.tpe).forall(isFullyInstantiated)
    case dt: DeclaredType if !dt.schema.isConstant => dt.typeArguments.forall(isFullyInstantiated)
    case _ => true
  }

  /**
    * Instantiates all inference variables in `tpe` with the respective bound.
    */
  def instantiateByBound(assignments: Assignments, tpe: Type, boundType: BoundType): Type = {
    instantiate(assignments, tpe, boundType, (bounds, boundType2) => bounds.get(boundType2))
  }

  /**
    * Instantiates all inference variables in `tpe` with a type given by `get`.
    */
  def instantiateCandidateType(assignments: Assignments, tpe: Type): Type = {
    // Note that the bound type is unimportant and just a dummy value here.
    instantiate(assignments, tpe, BoundType.Lower, (bounds, _) => bounds.candidateType)
  }

  /**
    * Instantiates all inference variables in `tpe` with a type given by `get`, which should take the bound type into
    * account. Contravariant types flip the bound type relationship, because their upper bounds effectively relate to
    * the lower bound of inference variables and vice versa.
    */
  private def instantiate(assignments: Assignments, tpe: Type, boundType: BoundType, get: (InferenceBounds, BoundType) => Type): Type = {
    // `instantiate` may be called with simple types quite often. We want to avoid reconstructing types (with all the
    // required allocations) in such cases.
    if (isFullyInstantiated(tpe)) {
      return tpe
    }

    val rec = (t: Type) => instantiate(assignments, t, boundType, get)
    val recContravariant = (t: Type) => instantiate(assignments, t, BoundType.flip(boundType), get)
    tpe match {
      case iv: InferenceVariable => get(InferenceVariable.bounds(iv, assignments), boundType)
      case SumType(types) => SumType.construct(types.map(rec))
      case IntersectionType(types) => IntersectionType.construct(types.map(rec))
      case TupleType(elements) => TupleType(elements.map(rec))
      case FunctionType(input, output) => FunctionType(recContravariant(input).asInstanceOf[TupleType], rec(output))
      case ListType(element) => ListType(rec(element))
      case MapType(key, value) => MapType(rec(key), rec(value))
      case shapeType: ShapeType => shapeType.mapPropertyTypes(rec)
      case dt: DeclaredType if !dt.schema.isConstant =>
        val newAssignments = dt.assignments.map { case (typeParameter, typeArgument) =>
          val typeArgument2 = typeParameter.variance match {
            case Variance.Covariant | Variance.Invariant => rec(typeArgument)
            case Variance.Contravariant => recContravariant(typeArgument)
          }
          (typeParameter, typeArgument2)
        }
        dt.schema.instantiate(newAssignments)
      case tpe => tpe
    }
  }

}
