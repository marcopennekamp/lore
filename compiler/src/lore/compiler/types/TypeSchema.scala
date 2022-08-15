package lore.compiler.types

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.types.TypeVariable.Variance

/**
  * A type schema is a type constructor, taking any number of type parameters. Type schemas without any parameters are
  * called "types".
  */
trait TypeSchema {

  // TODO (multi-import): Rename `parameters` to `typeParameters` and `arity` to `schemaArity`. The problem is that
  //                      these names might conflict with other things known about types, especially instantiated
  //                      types, such as function type arity.

  /**
    * The schema's type parameters in their order of declaration.
    */
  def parameters: Vector[TypeVariable]
  def arity: Int = parameters.length

  /**
    * A constant schema has no type parameters (arity 0) and is thus effectively equal to a single type.
    *
    * TODO (multi-import): Rename to `isConstantSchema`.
    */
  def isConstant: Boolean = arity == 0

  /**
    * Whether the schema has invariant type parameters.
    *
    * TODO (multi-import): Rename to `hasInvariantTypeParameters`.
    */
  lazy val hasInvariantParameters: Boolean = parameters.exists(_.variance == Variance.Invariant)

  /**
    * The constant type only exists if the schema is constant and is simply its constant representative. Before
    * accessing `constantType`, make sure that the schema is constant, otherwise a CompilationException will be thrown.
    */
  def constantType: Type = _constantType

  private lazy val _constantType: Type = {
    if (isConstant) instantiate(Map.empty)
    else throw CompilationException(s"Cannot get the constant type of a non-constant schema. Schema: $this.")
  }

  /**
    * These assignments can be used to instantiate the type schema with the parameters themselves as type arguments.
    * This is beneficial in a few select use cases.
    *
    * TODO (multi-import): Rename to `identityTypeParameterAssignments`.
    */
  lazy val identityAssignments: TypeVariable.Assignments = parameters.zip(parameters).toMap

  /**
    * Instantiates the schema with the given type variable assignments, which are already fully checked by the above
    * variant of the `instantiate` method, or otherwise deemed to be legal.
    *
    * For example, when a schema is instantiated for type inference, we do not necessarily know all types at the point
    * of instantiation. Instead of checking bounds like the `instantiate` method above, the schema's type variable
    * bounds are taken into account as typing judgments.
    */
  def instantiate(assignments: TypeVariable.Assignments): Type

  /**
    * Instantiates the schema with the given type argument list, which must be in the order of declaration of the
    * schema's type parameters. This implementation guarantees that all and only type parameters of the schema have a
    * corresponding assignment. It also ensures that type variable bounds are kept.
    *
    * If any of the constraints fail, None is returned.
    */
  def instantiate(arguments: Vector[Type]): Option[Type] = {
    var isValid = true
    val result = instantiate(arguments.map(Some(_)), () => isValid = false, (_, _) => isValid = false)
    if (isValid) Some(result) else None
  }

  /**
    * Implements the `instantiate` method above with the following additions:
    *
    *   - If the arity of the type arguments is incorrect or type variable bounds are not kept, appropriate errors are
    *     reported.
    *   - Missing type arguments default to the most general bound. Type arguments may be partially specified, for
    *     example when the type expression evaluator couldn't resolve the first type argument, but it could resolve the
    *     second type argument.
    *   - Even if the `instantiate` operation above would fail, this implementation produces a best-guess type instance
    *     so that follow-up type errors are kept at a minimum when the compiler continues compilation.
    */
  def instantiate(arguments: Vector[Option[Type]], position: Position)(implicit reporter: Reporter): Type = {
    instantiate(
      arguments,
      () => reporter.error(TypingFeedback.Schema.IllegalArity(this, arguments.length, position)),
      (tv, argument) => reporter.error(TypingFeedback.Schema.IllegalBounds(tv, argument, position)),
    )
  }

  private def instantiate(
    arguments: Vector[Option[Type]],
    illegalArity: () => Unit,
    illegalBounds: (TypeVariable, Type) => Unit,
  ): Type = {
    if (arguments.length != arity) {
      illegalArity()
    }

    if (isConstant) {
      return constantType
    }

    val assignments = parameters.zipWithIndex.foldLeft(Map.empty: TypeVariable.Assignments) { case (assignments, (parameter, index)) =>
      val lowerBound = Type.substitute(parameter.lowerBound, assignments)
      val upperBound = Type.substitute(parameter.upperBound, assignments)

      val argument = arguments.lift(index).flatten match {
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
  def getTypeScope(parentScope: TypeScope): TypeScope = ImmutableTypeScope.from(parameters, parentScope)

  override def toString: String = SchemaStringifier.toString(this)

}
