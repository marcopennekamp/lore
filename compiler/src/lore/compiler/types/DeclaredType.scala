package lore.compiler.types

import lore.compiler.types.TypeVariable.Variance
import lore.compiler.utils.CollectionExtensions.{MapVectorExtension, VectorExtension}

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
    * most general type arguments. In some cases, the type cannot be specialized to the given schema, in which case
    * None is returned.
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
    println(s"Specializing $this to $subtypeSchema.")

    val result = specializationAssignments(subtypeSchema)
      .flatMap(assignments => checkSpecializationParameters(subtypeSchema, assignments))
      .map(subtypeSchema.instantiate(_).asInstanceOf[DeclaredType])

    println(s"Specialization result: $result.")
    println()

    result
  }

  /**
    * For each extends clause in the subtype schema that extends this type's schema, we build a type variable
    * allocation that assigns types from this type's type arguments to the parameters of the subtype schema. If the
    * type parameter occurs multiple times in the extends clause, the candidate types are merged based on variance.
    * Each set of resulting assignments is valid if the instantiated extends clause is a subtype of this type.
    *
    * Once possible assignments have been determined, we combine them again by taking the variance of each type
    * parameter into account. The result is a type variable assignments map.
    *
    * If none of the extends clauses are valid, the function returns None. Such a result is different from an empty
    * assignments map: None signifies that there can be no subtype with the given schema because type arguments do not
    * agree, while an empty assignments map signifies that there can be a subtype, but none of its type parameters can
    * be derived from this type's arguments.
    *
    * Example: If we have a declared type `Cage[Fish]` and a subtype with extends clause `Cage[Unicorn]`, the extends
    * clause is not applicable, because `Unicorn </= Fish`. This restriction becomes very relevant when multiple type
    * arguments are involved. For example, consider a type `Function[String, Int]` and a subtype schema with extends
    * clauses `Function[String, T1]` and `Function[Real, T2]`. We can deduce `T1 = Int`, but <i>not</i> `T2 = Int`,
    * because the second extends clause has an incompatible type argument.
    */
  private def specializationAssignments(subtypeSchema: DeclaredSchema): Option[TypeVariable.Assignments] = {
    val extendsClauses = subtypeSchema.declaredSupertypes.filter(_.schema == this.schema)
    if (extendsClauses.isEmpty) {
      return None
    }

    def processClause(extendsClause: DeclaredType): Option[TypeVariable.Assignments] = {
      val candidates = TypeVariableAllocation.of(TupleType(this.typeArguments), TupleType(extendsClause.typeArguments)).allAssignments
      val assignments = candidates.map {
        case (parameter, types) =>
          val argument = combineCandidates(parameter, types.toSet).getOrElse {
            // If the candidates cannot be combined, this extends clause is not a relevant clause.
            return None
          }
          parameter -> argument
      }

      println(s"Extends clause: ${Type.substitute(extendsClause, assignments)} <= $this is ${Type.substitute(extendsClause, assignments) <= this}.")
      if (Type.substitute(extendsClause, assignments) <= this) {
        Some(assignments)
      } else {
        None
      }
    }

    val possibleAssignments = extendsClauses.flatMap(processClause)
    if (possibleAssignments.isEmpty) {
      return None
    }

    val assignments = possibleAssignments.merged.map {
      case (parameter, candidates) =>
        val argument = combineCandidates(parameter, candidates.toSet).getOrElse(
          // If candidates cannot be combined, we have a typing conflict and the subtype cannot be a subtype of this
          // declared type.
          return None
        )
        parameter -> argument
    }

    Some(assignments)
  }

  /**
    * Ensures that all parameters of the subtype schema have correctly assigned types:
    *   - If the schema's parameter does not exist in the assignments (i.e. it cannot be inferred from one of the
    *     extends clauses), we have to guess the most general type argument.
    *   - If the bounds of the parameter are narrower than the assignment candidate, we have to fall back to the lower
    *     or upper bound, as long as the parameter's variance allows it. If the parameter is invariant, the function
    *     returns None, as we cannot fall back to a bound and the subtyping relationship is impossible.
    *
    * Example: `Cage[Animal]` has the direct subtype `Aquarium[Fish]` (see `types.lore` test definitions), because
    * `Aquarium` can only contain Fish.
    */
  private def checkSpecializationParameters(subtypeSchema: DeclaredSchema, assignments: TypeVariable.Assignments): Option[TypeVariable.Assignments] = {
    // We have to keep track of the actual assignments from left to right so that we can instantiate a parameter's
    // lower and upper bound with the correct type arguments (if a bound depends on an earlier type parameter).
    val newAssignments = subtypeSchema.parameters.foldLeft(assignments) { (assignments, parameter) =>
      val lowerBound = Type.substitute(parameter.lowerBound, assignments)
      val upperBound = Type.substitute(parameter.upperBound, assignments)

      val argument = assignments.getOrElse(
        parameter,
        parameter.variance match {
          case Variance.Covariant => Type.substitute(parameter.upperBound, assignments)
          case Variance.Contravariant => Type.substitute(parameter.lowerBound, assignments)
          case Variance.Invariant =>
            // This must be the type parameter itself, because the most general type argument must represent all
            // possible type arguments, and only a type variable can do that if the parameter is invariant.
            parameter
        }
      )

      val boundedArgument = if (lowerBound </= argument) {
        if (parameter.variance == Variance.Contravariant) lowerBound
        else return None
      } else if (argument </= upperBound) {
        if (parameter.variance == Variance.Covariant) upperBound
        else return None
      } else {
        argument
      }

      assignments.updated(parameter, boundedArgument)
    }

    Some(newAssignments)
  }

  /**
    * Combine the given candidate types, which should be assigned to the given parameter, such that there is a single
    * type assignment.
    */
  private def combineCandidates(parameter: TypeVariable, candidates: Set[Type]): Option[Type] = {
    if (candidates.size == 1) {
      return candidates.headOption
    }

    parameter.variance match {
      case Variance.Covariant => Some(IntersectionType.construct(candidates))
      case Variance.Contravariant => Some(SumType.construct(candidates))
      case Variance.Invariant => None
    }
  }

}
