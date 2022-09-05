package lore.compiler.typing.unification

import lore.compiler.types.TypeVariable.Variance
import lore.compiler.types._
import lore.compiler.typing.unification.InferenceBounds.BoundType
import lore.compiler.utils.CollectionExtensions.Tuple2Extension

/**
  * Inference variables are unique-by-reference types that can stand in for other types in complex situations. They are
  * then resolved by possibly multiple steps of unification during type inference.
  *
  * An inference variable's lower and upper bound limit the types that may be assigned to [[InferenceBounds]]. The
  * inference variable's bounds may contain other inference variables. As [[InferenceAssignments]] become narrower,
  * each inference variable's legal bounds may also narrow.
  *
  * Expression types should never permanently contain inference variables. Their purpose is limited to a few complex
  * cases such as function/constructor call inference, which requires unification to infer type parameters (represented
  * as inference variables).
  */
class InferenceVariable(
  val name: Option[String],
  val lowerBound: Type,
  val upperBound: Type,
) extends Type {
  override def toString: String = s"<${name.getOrElse("iv")}>"
}

object InferenceVariable {

  def apply(name: String, lowerBound: Type, upperBound: Type): InferenceVariable = {
    new InferenceVariable(Some(name), lowerBound, upperBound)
  }

  /**
    * Assigns `lowerBound` to `iv`'s lower bound and `upperBound` to its upper bound. Returns None if the old and new
    * bounds are incompatible.
    */
  def assign(
    iv: InferenceVariable,
    lowerBound: Type,
    upperBound: Type,
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    assign(iv, lowerBound, BoundType.Lower, assignments)
      .flatMap(assign(iv, upperBound, BoundType.Upper, _))
  }

  /**
    * Assigns `bound` to `iv`'s lower and upper bound. Returns None if the old and new bounds are incompatible.
    */
  def assign(
    iv: InferenceVariable,
    bound: Type,
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    assign(iv, bound, bound, assignments)
  }

  /**
    * Assigns `bound` to the lower or upper bound of `iv`, depending on `boundType`. Returns None if the old and new
    * bounds are incompatible.
    */
  def assign(
    iv: InferenceVariable,
    bound: Type,
    boundType: BoundType,
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    val assignment = assignments.getEffective(iv)
    if (assignment.lower <= bound && bound <= assignment.upper) {
      val updatedBounds = boundType match {
        case BoundType.Lower => InferenceBounds(iv, bound, assignment.upper)
        case BoundType.Upper => InferenceBounds(iv, assignment.lower, bound)
      }
      Some(assignments.updated(iv, updatedBounds))
    } else None
  }

  /**
    * Ensures that `lowerBound` is a subtype of `iv`'s lower bound and `upperBound` is a supertype of `iv`'s upper
    * bound. If this is not the case, the respective bound will be assigned to `iv`.
    */
  def ensure(
    iv: InferenceVariable,
    lowerBound: Type,
    upperBound: Type,
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    ensure(iv, lowerBound, BoundType.Lower, assignments)
      .flatMap(ensure(iv, upperBound, BoundType.Upper, _))
  }

  /**
    * Ensures that `bound` is a subtype/supertype of `iv`'s lower or upper bound, depending on `boundType`. If this is
    * not the case, `bound` will be assigned to `iv`.
    *
    * In practical terms, the function thus assures that a given subtyping relationship holds, either by validating it
    * directly or by changing the bounds to "make it fit".
    */
  def ensure(
    iv: InferenceVariable,
    bound: Type,
    boundType: BoundType,
    assignments: InferenceAssignments,
  ): Option[InferenceAssignments] = {
    val assignment = assignments.getEffective(iv)

    boundType match {
      case BoundType.Lower => if (bound <= assignment.lower) return Some(assignments)
      case BoundType.Upper => if (assignment.upper <= bound) return Some(assignments)
    }

    assign(iv, bound, boundType, assignments)
  }

  /**
    * Whether the given type contains no inference variables at all.
    */
  def isFullyInstantiated(tpe: Type): Boolean = tpe match {
    case _: InferenceVariable => false
    case SumType(types) => types.forall(isFullyInstantiated)
    case IntersectionType(types) => types.forall(isFullyInstantiated)
    case TupleType(elements) => elements.forall(isFullyInstantiated)
    case FunctionType(input, output) => isFullyInstantiated(input) && isFullyInstantiated(output)
    case ListType(element) => isFullyInstantiated(element)
    case MapType(key, value) => isFullyInstantiated(key) && isFullyInstantiated(value)
    case tpe: ShapeType => tpe.propertyTypes.forall(isFullyInstantiated)
    case dt: DeclaredType if !dt.schema.isConstantSchema => dt.typeArguments.forall(isFullyInstantiated)
    case _ => true
  }

  /**
    * Substitutes in `tpe` all type variables from `typeVariables` with inference variables, returning the result type
    * and a type variable assignments map.
    */
  def fromTypeVariables(
    tpe: Type,
    typeVariables: Vector[TypeVariable],
  ): (Type, Map[TypeVariable, InferenceVariable]) = {
    fromTypeVariables(Vector(tpe), typeVariables).mapFirst { case Vector(result) => result }
  }

  /**
    * Substitutes in `types` all type variables from `typeVariables` with inference variables, returning the result
    * types and a type variable assignments map.
    */
  def fromTypeVariables(
    types: Vector[Type],
    typeVariables: Vector[TypeVariable],
  ): (Vector[Type], Map[TypeVariable, InferenceVariable]) = {
    val tvToIv = typeVariables.foldLeft(Map.empty[TypeVariable, InferenceVariable]) { case (tvToIv, tv) =>
      val lowerBound = Type.substitute(tv.lowerBound, tvToIv)
      val upperBound = Type.substitute(tv.upperBound, tvToIv)
      tvToIv + (tv -> InferenceVariable(tv.simpleName, lowerBound, upperBound))
    }
    val resultType = types.map(Type.substitute(_, tvToIv))
    (resultType, tvToIv)
  }

  def toTypeVariableAssignments(
    tvToIv: Map[TypeVariable, InferenceVariable],
    assignments: InferenceAssignments,
  ): TypeVariable.Assignments = tvToIv.map {
    case (tv, iv) => tv -> InferenceVariable.instantiateCandidate(iv, assignments)
  }

  /**
    * Instantiates all inference variables in `tpe` with the respective bound.
    */
  def instantiateByBound(tpe: Type, boundType: BoundType, assignments: InferenceAssignments): Type = {
    instantiate(tpe, boundType, assignments)((bounds, boundType2) => bounds.get(boundType2))
  }

  /**
    * Instantiates all inference variables in `tpe` with their candidate types.
    */
  def instantiateCandidate(tpe: Type, assignments: InferenceAssignments): Type = {
    // Note that the bound type determines whether the candidate type is instantiated with a default of Nothing or Any,
    // if an inference variable doesn't have a candidate type.
    instantiate(tpe, BoundType.Upper, assignments) {
      (bounds, boundType) => bounds.candidateType match {
        case Some(candidateType) => candidateType
        case None => boundType match {
          case BoundType.Lower => BasicType.Nothing
          case BoundType.Upper => BasicType.Any
        }
      }
    }
  }

  /**
    * Instantiates all inference variables in `tpe` with a type given by `get`, which should take the bound type into
    * account. Contravariant types flip the bound type relationship, because their upper bounds effectively relate to
    * the lower bound of inference variables and vice versa.
    */
  private def instantiate(
    tpe: Type,
    boundType: BoundType,
    assignments: InferenceAssignments,
  )(get: (InferenceBounds, BoundType) => Type): Type = {
    // `instantiate` may be called with simple types quite often. We want to avoid reconstructing types (with all the
    // required allocations) in such cases.
    if (isFullyInstantiated(tpe)) {
      return tpe
    }

    val rec = (t: Type) => instantiate(t, boundType, assignments)(get: (InferenceBounds, BoundType) => Type)
    val recContravariant = (t: Type) => instantiate(t, BoundType.flip(boundType), assignments)(get)
    tpe match {
      case iv: InferenceVariable => get(assignments.getEffective(iv), boundType)
      case SumType(types) => SumType.construct(types.map(rec))
      case IntersectionType(types) => IntersectionType.construct(types.map(rec))
      case TupleType(elements) => TupleType(elements.map(rec))
      case FunctionType(input, output) => FunctionType(recContravariant(input).asInstanceOf[TupleType], rec(output))
      case ListType(element) => ListType(rec(element))
      case MapType(key, value) => MapType(rec(key), rec(value))
      case shapeType: ShapeType => shapeType.mapPropertyTypes(rec)
      case dt: DeclaredType if !dt.schema.isConstantSchema =>
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
