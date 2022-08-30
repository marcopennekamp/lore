package lore.compiler.types

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.types.TypeVariable.Variance

/**
  * A type schema constructs types given a number of type arguments. Type schemas without type parameters are simply
  * called "types".
  */
trait TypeSchema {

  def typeParameters: Vector[TypeVariable]
  def schemaArity: Int = typeParameters.length

  /**
    * A constant schema has no type parameters (arity 0) and is thus effectively equal to a single type.
    */
  def isConstantSchema: Boolean = schemaArity == 0

  /**
    * Whether the schema has invariant type parameters.
    */
  lazy val hasInvariantTypeParameters: Boolean = typeParameters.exists(_.variance == Variance.Invariant)

  /**
    * The constant type only exists if the schema is constant and is simply its constant representative. Before
    * accessing `constantType`, make sure that the schema is constant, otherwise a CompilationException will be thrown.
    */
  def constantType: Type = _constantType

  // TODO (multi-import): Can't this simply be inlined into the `constantType` definition?
  private lazy val _constantType: Type = {
    if (isConstantSchema) instantiate(Map.empty)
    else throw CompilationException(s"Cannot get the constant type of a non-constant schema. Schema: $this.")
  }

  /**
    * These assignments can be used to instantiate the type schema with the parameters themselves as type arguments.
    * This is beneficial in a few select use cases.
    *
    * TODO (multi-import): Is this still used enough? Seems kinda specific.
    */
  lazy val typeParameterIdentityAssignments: TypeVariable.Assignments = typeParameters.zip(typeParameters).toMap

  /**
    * Instantiates the schema with the given type variable assignments, which must already be fully checked by a safe
    * variant of the [[instantiate]] method, or otherwise deemed to be legal.
    *
    * For example, when a schema is instantiated for type inference, we do not necessarily know all types at the point
    * of instantiation. Instead of checking bounds like other [[instantiate]] methods, the schema's type variable
    * bounds are ensured by type variable unification.
    */
  def instantiate(assignments: TypeVariable.Assignments): Type

  /**
    * Instantiates the schema with the given type arguments, which must be in the order of declaration of the schema's
    * type parameters. This implementation guarantees that all and only type parameters of the schema have a
    * corresponding assignment. It also ensures that type variable bounds are kept.
    *
    * If any of the constraints fail, `None` is returned.
    */
  def instantiate(typeArguments: Vector[Type]): Option[Type] = {
    var isValid = true
    val result = instantiate(typeArguments.map(Some(_)), () => isValid = false, (_, _) => isValid = false)
    if (isValid) Some(result) else None
  }

  /**
    * Implements the [[instantiate]] method for type arguments <i>with</i> error reporting:
    *
    *   - If the arity of the type arguments is incorrect or type variable bounds are not kept, appropriate errors are
    *     reported.
    *   - Even if the other [[instantiate]] method would fail, this implementation produces a best-guess type instance
    *     so that follow-up type errors are kept at a minimum when the compiler continues compilation.
    *   - Missing type arguments default to the most general bound. Type arguments may be partially specified, for
    *     example when the type expression evaluator couldn't resolve the first type argument, but it could resolve the
    *     second type argument.
    */
  def instantiate(typeArguments: Vector[Option[Type]], position: Position)(implicit reporter: Reporter): Type = {
    instantiate(
      typeArguments,
      () => reporter.error(TypingFeedback.Schema.IllegalArity(this, typeArguments.length, position)),
      (tv, argument) => reporter.error(TypingFeedback.IllegalBounds(argument, tv, position)),
    )
  }

  private def instantiate(
    typeArguments: Vector[Option[Type]],
    illegalArity: () => Unit,
    illegalBounds: (TypeVariable, Type) => Unit,
  ): Type = {
    if (typeArguments.length != schemaArity) {
      illegalArity()
    }

    if (isConstantSchema) {
      return constantType
    }

    val assignments = typeParameters.zipWithIndex.foldLeft(Map.empty: TypeVariable.Assignments) {
      case (assignments, (parameter, index)) =>
        val lowerBound = Type.substitute(parameter.lowerBound, assignments)
        val upperBound = Type.substitute(parameter.upperBound, assignments)

        val argument = typeArguments.lift(index).flatten match {
          case Some(argument) => argument

          // If the parameter has no corresponding argument, we take a type variable bounds guess at the actual type.
          case None => parameter.variance match {
            case Variance.Covariant | Variance.Invariant => upperBound
            case Variance.Contravariant => lowerBound
          }
        }

        if ((lowerBound </= argument) || (argument </= upperBound)) {
          illegalBounds(parameter, argument)
        }

        assignments + (parameter -> argument)
    }

    instantiate(assignments)
  }

  /**
    * Creates an immutable type scope that allows access to the type schema's type parameters.
    */
  def getTypeScope(parentScope: TypeScope): TypeScope = ImmutableTypeScope.from(typeParameters, parentScope)

  override def toString: String = SchemaStringifier.toString(this)

}
