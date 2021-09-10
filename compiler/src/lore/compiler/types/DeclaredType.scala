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
    * If this type's schema has multiple parameterized inheritance and the supertype schema has type parameters, the
    * result's type arguments are each a combination of all occurring type argument candidates according to variance:
    *
    *   - If a type parameter is covariant, the resulting type argument is the intersection type of all candidates.
    *   - If a type parameter is contravariant, the resulting type argument is the sum type of all candidates.
    *   - If a type parameter is invariant, all candidates must be equal. Otherwise, `findSupertype` results in None.
    *
    * For example, if we have a type with extends clauses `Cage[Animal]`, `Cage[Fish]`, and `Cage[Unicorn]`, the
    * resulting declared supertype will be `Cage[Fish & Unicorn]`.
    */
  def findSupertype(supertypeSchema: DeclaredSchema): Option[DeclaredType] = {
    if (!schema.hasMultipleParameterizedInheritance || supertypeSchema.isConstant) {
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
    Type.logger.trace(s"Specializing $this to $subtypeSchema.")

    val result = specializationAssignments(subtypeSchema)
      .flatMap(assignments => checkSpecializationParameters(subtypeSchema, assignments))
      .map(subtypeSchema.instantiate(_).asInstanceOf[DeclaredType])

    Type.logger.trace(s"Specialization result: $result.")
    Type.loggerBlank.trace("")

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

    def processCandidates(candidates: Map[TypeVariable, Vector[Type]]): Option[Map[TypeVariable, Type]] = {
      val result = candidates.map {
        case (parameter, types) =>
          val argument = combineCandidates(parameter, types.toSet).getOrElse {
            // If candidates cannot be combined, we have a typing conflict and the extends clause is invalid, or a
            // subtype cannot be a subtype of this declared type.
            Type.logger.trace(s"Candidates cannot be combined: ${candidates.toSet}")
            return None
          }
          parameter -> argument
      }
      Some(result)
    }

    def processClause(extendsClause: DeclaredType): Option[TypeVariable.Assignments] = {
      val candidates = TypeVariableAllocation.of(TupleType(this.typeArguments), TupleType(extendsClause.typeArguments)).allAssignments
      processCandidates(candidates).flatMap { assignments =>
        val instantiatedClause = Type.substitute(extendsClause, assignments)
        if (instantiatedClause fits this) {
          Type.logger.trace(s"Extends clause: $instantiatedClause fits $this.")
          Some(assignments)
        } else {
          Type.logger.trace(s"Extends clause: $instantiatedClause does not fit $this.")
          None
        }
      }
    }

    val possibleAssignments = extendsClauses.flatMap(processClause)
    Type.logger.trace(s"Possible assignments: $possibleAssignments")
    if (possibleAssignments.isEmpty) {
      return None
    }

    processCandidates(possibleAssignments.merged)
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
          case Variance.Covariant => upperBound
          case Variance.Contravariant => lowerBound
          case Variance.Invariant =>
            // This must be the type parameter itself, because the most general type argument must represent all
            // possible type arguments, and only a type variable can do that if the parameter is invariant.
            parameter
        }
      )

      // We have to take special care when the type argument is a type variable. The goal here is to narrow the type
      // argument so that it's compatible with the type parameter's bounds. Subtype checks are not sufficient when the
      // type argument is a type variable itself.
      // For example, if we're specializing a type `Cage[X]` with `X <: Animal` to a schema `Aquarium[A]` with
      // `A <: Fish`, we set `A = X`. But X could be `Animal`, which isn't a `Fish`. A is narrower than X. Hence, we
      // have to narrow the type argument to `Fish`. To find this out for type variables, we check that `Fish` fits
      // into X (proving that X is equally specific or more general than A) and that X does not fit into `Fish`
      // (proving that X and A are not equally specific).
      // TODO: The above considerations also have to be applied to lower bounds. We definitely need to apply Fit there
      //       too. But it seems like we'd need a Fit implementation that assigns types from `lowerBound` to variables
      //       in `argument`, but also checks `argument <= lowerBound` instead of the usual `lowerBound <= argument`.
      val boundedArgument = if (argument < lowerBound && parameter.variance == Variance.Contravariant) {
        lowerBound
      } else if ((upperBound fits argument) && (argument fitsNot upperBound) && parameter.variance == Variance.Covariant) {
        upperBound
      } else if (lowerBound </= argument) {
        Type.logger.trace(s"Lower bound $lowerBound doesn't agree with type argument $argument.")
        return None
      } else if (argument </= upperBound) {
        Type.logger.trace(s"Upper bound $upperBound doesn't agree with type argument $argument.")
        return None
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
