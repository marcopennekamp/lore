package lore.compiler.typing

import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.types._
import lore.compiler.typing.InferenceBounds.BoundType

import java.util.concurrent.atomic.AtomicInteger

/**
  * Inference variables are strictly reference-equal, much like type variables. They can be put in types in place of
  * other types and are resolved by the type inference algorithm.
  */
class InferenceVariable(val name: Option[String] = None) extends Type {
  override def equals(obj: Any): Boolean = obj match {
    case var2: InferenceVariable => this eq var2
    case _ => false
  }

  lazy val label: String = name.getOrElse {
    s"iv${InferenceVariable.nameCounter.incrementAndGet()}"
  }

  override def toString: String = label
}

object InferenceVariable {

  private val nameCounter: AtomicInteger = new AtomicInteger()

  type Assignments = Map[InferenceVariable, InferenceBounds]

  implicit class AssignmentsExtension(assignments: Assignments) {
    /**
      * Instantiates the candidate types of all inference variables in `tpe`.
      *
      * This function is intended to be used by other compiler components, not within the `typing` package.
      */
    def instantiate(tpe: Type): Type = instantiateCandidate(tpe, assignments)

    def stringified: String = assignments.values.toVector.sortBy(_.variable.label).mkString("\n")
  }

  /**
    * Returns the effective bounds of `iv`, which may or may not be contained in `assignments`.
    */
  def getBounds(iv: InferenceVariable, assignments: Assignments): InferenceBounds = {
    assignments.getOrElse(iv, InferenceBounds(iv, BasicType.Nothing, BasicType.Any))
  }

  /**
    * Assigns `lowerBound` to `iv`'s lower bound and `upperBound` to its upper bound. Returns None if the old and new
    * bounds are incompatible.
    */
  def assign(
    iv: InferenceVariable,
    lowerBound: Type,
    upperBound: Type,
    assignments: Assignments,
  ): Option[Assignments] = {
    assign(iv, lowerBound, BoundType.Lower, assignments)
      .flatMap(assign(iv, upperBound, BoundType.Upper, _))
  }

  /**
    * Assigns `bound` to `iv`'s lower and upper bound. Returns None if the old and new bounds are incompatible.
    */
  def assign(
    iv: InferenceVariable,
    bound: Type,
    assignments: Assignments,
  ): Option[Assignments] = {
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
    assignments: Assignments,
  ): Option[Assignments] = {
    val bounds = getBounds(iv, assignments)
    if (bounds.lower <= bound && bound <= bounds.upper) {
      val updatedBounds = boundType match {
        case BoundType.Lower => InferenceBounds(iv, bound, bounds.upper)
        case BoundType.Upper => InferenceBounds(iv, bounds.lower, bound)
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
    assignments: Assignments,
  ): Option[Assignments] = {
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
    assignments: Assignments,
  ): Option[Assignments] = {
    val bounds = getBounds(iv, assignments)

    boundType match {
      case BoundType.Lower => if (bound <= bounds.lower) return Some(assignments)
      case BoundType.Upper => if (bounds.upper <= bound) return Some(assignments)
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
    case ShapeType(properties) => properties.values.map(_.tpe).forall(isFullyInstantiated)
    case dt: DeclaredType if !dt.schema.isConstant => dt.typeArguments.forall(isFullyInstantiated)
    case _ => true
  }

  /**
    * Substitutes in `tpe` all type variables from `typeVariables` with inference variables, returning the result type
    * and a type variable assignments map.
    */
  def fromTypeVariables(tpe: Type, typeVariables: Vector[TypeVariable]): (Type, Map[TypeVariable, InferenceVariable]) = {
    val typeVariableAssignments = typeVariables.map(tv => (tv, new InferenceVariable)).toMap
    val resultType = Type.substitute(tpe, typeVariableAssignments)
    (resultType, typeVariableAssignments)
  }

  /**
    * Instantiates all inference variables in `tpe` with the respective bound.
    */
  def instantiateByBound(tpe: Type, boundType: BoundType, assignments: Assignments): Type = {
    instantiate(tpe, boundType, assignments)((bounds, boundType2) => bounds.get(boundType2))
  }

  /**
    * Instantiates all inference variables in `tpe` with their candidate types.
    */
  def instantiateCandidate(tpe: Type, assignments: Assignments): Type = {
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
    * Instantiates all inference variables in the type of `expression` with their candidate types.
    */
  def instantiateCandidate(expression: Expression, assignments: Assignments): Type = {
    instantiateCandidate(expression.tpe, assignments)
  }

  /**
    * Instantiates all inference variables in all types of `expressions` with their candidate types.
    */
  def instantiateCandidate(expressions: Vector[Expression], assignments: Assignments): Vector[Type] = {
    expressions.map(instantiateCandidate(_, assignments))
  }

  /**
    * Instantiates all inference variables in `tpe` with a type given by `get`, which should take the bound type into
    * account. Contravariant types flip the bound type relationship, because their upper bounds effectively relate to
    * the lower bound of inference variables and vice versa.
    */
  private def instantiate(tpe: Type, boundType: BoundType, assignments: Assignments)(get: (InferenceBounds, BoundType) => Type): Type = {
    // `instantiate` may be called with simple types quite often. We want to avoid reconstructing types (with all the
    // required allocations) in such cases.
    if (isFullyInstantiated(tpe)) {
      return tpe
    }

    val rec = (t: Type) => instantiate(t, boundType, assignments)(get: (InferenceBounds, BoundType) => Type)
    val recContravariant = (t: Type) => instantiate(t, BoundType.flip(boundType), assignments)(get)
    tpe match {
      case iv: InferenceVariable => get(InferenceVariable.getBounds(iv, assignments), boundType)
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
