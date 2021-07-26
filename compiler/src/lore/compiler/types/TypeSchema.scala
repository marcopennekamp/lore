package lore.compiler.types

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}

/**
  * A type schema is a type constructor, taking any number of type parameters. Type schemas without any parameters are
  * called "types".
  */
trait TypeSchema {
  /**
    * The schema's type parameters in their order of declaration.
    */
  def parameters: Vector[TypeVariable]
  def arity: Int = parameters.length

  /**
    * A constant schema has no type parameters (arity 0) and is thus effectively equal to a single type.
    */
  def isConstant: Boolean = arity == 0

  /**
    * Creates an immutable type scope that allows access to the type schema's type parameters.
    */
  def getTypeScope(parentScope: TypeScope): TypeScope = ImmutableTypeScope.from(parameters, parentScope)

  /**
    * Instantiates the schema with the given type argument list, which must be in the order of declaration of the
    * schema's type parameters. This implementation guarantees that all and only type parameters of the schema have a
    * corresponding assignment. It also ensures that type variable bounds are kept.
    */
  def instantiate(arguments: Vector[Type], position: Position)(implicit reporter: Reporter): Option[Type] = {
    if (arguments.length != arity) {
      reporter.error(TypeSchema.IllegalArity(this, arguments.length, position))
      return None
    }

    if (isConstant) {
      return Some(instantiateConstant())
    }

    val assignments = parameters.zip(arguments).toMap

    var boundsKept = true
    for ((tv, argument) <- assignments) {
      if ((tv.lowerBound </= argument) || (argument </= tv.upperBound)) {
        reporter.error(TypeSchema.IllegalBounds(tv, argument, position))
        boundsKept = false
      }
    }

    if (boundsKept) Some(instantiate(assignments))
    else None
  }

  /**
    * Instantiates the schema as a constant type, but only if the schema takes no parameters.
    */
  def instantiateConstant(): Type = {
    if (!isConstant) {
      throw CompilationException(s"Cannot instantiate a non-constant schema $this as constant.")
    }
    instantiate(Map.empty)
  }

  /**
    * Instantiates the schema with the given type variable assignments, which are already fully checked by the above
    * variant of the `instantiate` method, or otherwise deemed to be legal.
    *
    * For example, when a schema is instantiated for type inference, we do not necessarily know all types at the point
    * of instantiation. Instead of checking bounds like the `instantiate` method above, the schema's type variable
    * bounds are taken into account as typing judgments.
    */
  def instantiate(assignments: TypeVariable.Assignments): Type
}

object TypeSchema {

  case class IllegalArity(schema: TypeSchema, arity: Int, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The type $schema expects ${schema.arity} type arguments, but $arity type" +
      s" arguments were supplied."
  }

  case class IllegalBounds(tv: TypeVariable, argument: Type, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The type argument $argument must adhere to the lower bound ${tv.lowerBound} and" +
      s" the upper bound ${tv.upperBound}."
  }

}
