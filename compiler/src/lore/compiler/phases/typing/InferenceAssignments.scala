package lore.compiler.phases.typing

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.phases.typing.Inference.{InvalidLowerBound, InvalidUpperBound}
import lore.compiler.types.{BasicType, FunctionType, IntersectionType, ListType, MapType, ProductType, ShapeType, Subtyping, SumType, Type, TypeVariable}

object InferenceAssignments {

  type Assignments = Map[InferenceVariable, Bounds]

  case class Bounds(variable: InferenceVariable, lower: Type, upper: Type)

  implicit class AssignmentsExtension(assignments: Assignments) {

    def isDefined(inferenceVariable: InferenceVariable): Boolean = assignments.contains(inferenceVariable)

    /**
      * Instantiate all inference variables in the given type with their corresponding upper bounds.
      */
    def instantiate(tpe: Type): Type = tpe match {
      case iv: InferenceVariable =>
        assignments.getOrElse(iv, throw CompilationException(s"The inference variable $iv should be defined.")).upper
      case tv: TypeVariable =>
        if (Inference.isFullyInferred(tv)) tv
        else ??? // TODO: How can we instantiate the type variable without destroying its reference equality? Maybe a type variable requires a UUID instead?
      case SumType(types) => SumType.construct(types.map(instantiate))
      case IntersectionType(types) => IntersectionType.construct(types.map(instantiate))
      case ProductType(elements) => ProductType(elements.map(instantiate))
      case FunctionType(input, output) => FunctionType(instantiate(input), instantiate(output))
      case ListType(element) => ListType(instantiate(element))
      case MapType(key, value) => MapType(instantiate(key), instantiate(value))
      case shapeType: ShapeType => shapeType.mapPropertyTypes(instantiate)
      case tpe => tpe
    }

    /**
      * Assigns the given lower and upper bounds to the given inference variable. If the lower bound is None, any lower
      * bound already present is carried over into the new bounds.
      */
    def assignBounds(inferenceVariable: InferenceVariable, lower: Option[Type], upper: Type, context: TypingJudgment): Compilation[Assignments] = {
      val bounds = assignments.getOrElse(inferenceVariable, Bounds(inferenceVariable, BasicType.Nothing, BasicType.Any))

      lower.map { lower =>
        if (!Subtyping.isSubtype(bounds.lower, lower)) {
          Compilation.fail(InvalidLowerBound(inferenceVariable, lower, bounds.lower, context))
        } else Verification.succeed
      }.toCompiledOption.flatMap { _ =>
        if (!Subtyping.isSubtype(upper, bounds.upper)) {
          Compilation.fail(InvalidUpperBound(inferenceVariable, upper, bounds.upper, context))
        } else Verification.succeed
      }.map { _ =>
        assignments.updated(inferenceVariable, Bounds(inferenceVariable, lower.getOrElse(bounds.lower), upper))
      }
    }

  }

}
