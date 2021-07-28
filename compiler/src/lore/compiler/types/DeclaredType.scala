package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.utils.CollectionExtensions.{MapExtension, VectorExtension}

trait DeclaredType extends NamedType {

  /**
    * The schema this declared type was instantiated from.
    */
  def schema: DeclaredSchema

  override def name: String = schema.name

  /**
    * The type arguments this declared type has been instantiated with.
    */
  def assignments: TypeVariable.Assignments

  lazy val typeArguments: Vector[Type] = schema.parameters.map(Type.substitute(_, assignments))

  /**
    * All direct declared supertypes of the declared type. Any type arguments of the declared type are found in their
    * instantiated versions.
    */
  lazy val declaredSupertypes: Vector[DeclaredType] = schema.declaredSupertypes.map(Type.substitute(_, assignments).asInstanceOf[DeclaredType])

  /**
    * The inherited shape type of the declared type is derived from the schema's inherited shape type, but with all
    * type parameters instantiated given the current type arguments.
    */
  lazy val inheritedShapeType: ShapeType = Type.substitute(schema.inheritedShapeType, assignments).asInstanceOf[ShapeType]

  /**
    * The declared type viewed as a compile-time shape type. By default, this is equal to the [[inheritedShapeType]].
    * Structs, however, implement `asShapeType` based on their properties.
    */
  def asShapeType: ShapeType = inheritedShapeType

  /**
    * A set of <i>all</i> the type's direct and indirect supertypes. Does not contain duplicates, but may contain
    * multiple types of the same schema with different type arguments.
    */
  lazy val indirectDeclaredSupertypes: Set[DeclaredType] = {
    declaredSupertypes.toSet.flatMap((supertype: DeclaredType) => Set(supertype) ++ supertype.indirectDeclaredSupertypes)
  }

  /**
    * Finds a supertype of this type (or this type itself) that has the given schema.
    *
    * If this type's schema has multiple parameterized inheritance, the result's type arguments are each a combination
    * of all occurring type argument candidates according to variance:
    *
    *   - If a type parameter is covariant, the resulting type argument is the intersection type of all candidates.
    *   - If a type parameter is contravariant, the resulting type argument is the sum type of all candidates.
    *   - If a type parameter is invariant, all candidates must be equal. Otherwise, `findSupertype` results in None.
    *
    * For example, if we have a type `Cage[+A]` with the candidates `Cage[Animal]`, `Cage[Fish]`, and `Cage[Unicorn]`,
    * the resulting declared type will be `Cage[Fish & Unicorn]`.
    */
  def findSupertype(supertypeSchema: DeclaredSchema): Option[DeclaredType] = {
    if (!schema.hasMultipleParameterizedInheritance) {
      if (this.schema == supertypeSchema) Some(this)
      else declaredSupertypes.firstDefined(_.findSupertype(supertypeSchema))
    } else {
      def collect(dt: DeclaredType): Vector[DeclaredType] = {
        if (dt.schema == supertypeSchema) Vector(dt)
        else dt.declaredSupertypes.flatMap(collect)
      }

      collect(this) match {
        case Vector() => None
        case Vector(candidate) => Some(candidate)
        case candidates =>
          val combinedArguments = (0 until supertypeSchema.arity).toVector.map { index =>
            val parameter = supertypeSchema.parameters(index)
            val arguments = candidates.map(_.typeArguments(index))
            parameter.variance match {
              case Variance.Covariant => IntersectionType.construct(arguments)
              case Variance.Contravariant => SumType.construct(arguments)
              case Variance.Invariant => if (arguments.toSet.size == 1) arguments.head else return None
            }
          }
          supertypeSchema.instantiate(combinedArguments).asInstanceOf[Option[DeclaredType]]
      }
    }
  }

  /**
    * Specializes this declared type to a <b>direct subtype</b> specified by the given subtype schema, choosing the
    * most general type arguments.
    *
    * The general approach here is to consider the actual type arguments of this declared type. We substitute them into
    * all of its occurrences in the `extends` clause of the subtype schema, which then allows us to deduce all or part
    * of the type arguments with which the subtype schema should be instantiated. The type arguments that cannot be
    * deduced are filled with the most general possible type. In some cases, the type cannot be specialized to the
    * given schema, in which case None is returned.
    *
    * This type <b>must</b> occur at least once in the `extends` clause of the subtype schema. If that is not the case,
    * a compilation exception is thrown.
    *
    * Example: Consider types `trait A[X <: Animal, +Z <: Animal]` and `trait B[X, Y <: Animal, +Z <: Mammal] extends
    * A[X, Z]`. Assume we want to find the direct declared subtypes of `A[Bird, Animal]`. We can directly match `Bird`
    * to B's `X` and `Animal` to B's `Z`. However, because B's upper bound of `Z` is covariant and narrower than
    * `Animal`, we have `Z = Mammal`. `Y` cannot be derived from `A` and so we have to instantiate it based on variance
    * alone. As `Y` is invariant, we have no choice but to set `Y = Y`. Ultimately, the direct subtype this function
    * returns will be `B[Bird, Y, Mammal]`. This is the type that will be used to check whether the totality constraint
    * is satisfied for an abstract function.
    */
  def specialize(subtypeSchema: DeclaredSchema): Option[DeclaredType] = {
    val extendClauses = subtypeSchema.declaredSupertypes.filter(_.schema == this.schema)
    if (extendClauses.isEmpty) {
      throw CompilationException(s"The declared type $this cannot be specified to the following schema: $subtypeSchema." +
        s" The schema does not directly extend $name.")
    }

    if (subtypeSchema.isConstant) {
      return Some(subtypeSchema.representative)
    }

    // Find all instantiations of the subtype schema's type parameters by substituting this type's actual type
    // arguments into the type parameters of the subtype schema.
    val occurrences = extendClauses.foldLeft(Map.empty[TypeVariable, Vector[Type]]) {
      case (existingOccurrences, extendedType) =>
        val candidates = TypeVariableAllocation.of(TupleType(this.typeArguments), TupleType(extendedType.typeArguments)).allAssignments
        existingOccurrences.mergeWith(candidates)
    }.distinct

    // We have to keep track of the actual assignments from left to right so that we can instantiate a parameter's
    // lower and upper bound with the correct type arguments (if a bound depends on an earlier type parameter).
    val assignments = subtypeSchema.parameters.foldLeft(Map.empty: TypeVariable.Assignments) { case (assignments, parameter) =>
      val lowerBound = Type.substitute(parameter.lowerBound, assignments)
      val upperBound = Type.substitute(parameter.upperBound, assignments)

      val argument = occurrences.get(parameter) match {
        // Case 1: The subtype schema's type parameter occurs in at least one position and has received an assignment
        //         from one of this type's type arguments.
        case Some(candidates) =>
          val candidate = if (candidates.length == 1) candidates.head else {
            // If there are multiple candidates, we have to combine them into a single assignment.
            parameter.variance match {
              case Variance.Covariant => IntersectionType.construct(candidates)
              case Variance.Contravariant => SumType.construct(candidates)
              case Variance.Invariant =>
                // Any type `S` instantiated via the subtype schema cannot be a subtype of this type, because `S` is
                // invariant in the type parameter and cannot be instantiated with two or more distinct types.
                return None
            }
          }

          // If the parameter's bounds are narrower than the candidate, we have to fall back to one of the bounds, as
          // long as the type parameter's variance allows it.
          // Example: `Cage[Animal]` has the direct subtype `Aquarium[Fish]` (see `types.lore` test definitions),
          //          because `Aquarium` can only contain Fish.
          if (lowerBound </= candidate) {
            if (parameter.variance == Variance.Contravariant) lowerBound
            else return None
          } else if (candidate </= upperBound) {
            if (parameter.variance == Variance.Covariant) upperBound
            else return None
          } else {
            candidate
          }

        // Case 2: The subtype schema's type parameter does not occur in any of the relevant `extends` clauses. We have
        //         to guess the most general type argument.
        case None => parameter.variance match {
          case Variance.Covariant => upperBound
          case Variance.Contravariant => lowerBound
          case Variance.Invariant =>
            // This must be the type parameter itself, because the most general type argument must represent all
            // possible type arguments, and only a type variable can do that if the parameter is invariant.
            parameter
        }
      }

      assignments + (parameter -> argument)
    }

    Some(subtypeSchema.instantiate(assignments).asInstanceOf[DeclaredType])
  }

}
