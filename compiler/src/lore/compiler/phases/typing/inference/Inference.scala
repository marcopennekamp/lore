package lore.compiler.phases.typing.inference

import lore.compiler.core.CompilationException
import lore.compiler.phases.typing.inference.InferenceBounds.BoundType
import lore.compiler.types._

object Inference {

  type Assignments = Map[InferenceVariable, InferenceBounds]

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
  def isFullyInferred(tpe: Type): Boolean = variables(tpe).isEmpty

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

}
