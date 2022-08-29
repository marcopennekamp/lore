package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.ParameterDefinition.NamedParameterView
import lore.compiler.types._

case class FunctionSignature(
  name: NamePath,
  typeParameters: Vector[TypeVariable],
  parameters: Vector[ParameterDefinition],
  outputType: Type,
  position: Position,
) extends FunctionLike with Positioned {
  val namedParameters: Vector[NamedParameterView] = parameters.filter(_.name.isDefined).map(NamedParameterView)

  override val parameterTypes: Vector[Type] = parameters.map(_.tpe)
  override val asFunctionType: FunctionType = super.asFunctionType

  override def toString: String = s"$name${super.toString}"

  /**
    * Substitutes the given type variable assignments into the parameter types and output type and returns a new
    * monomorphic function signature.
    */
  def substitute(assignments: TypeVariable.Assignments): FunctionSignature = {
    if (assignments.isEmpty) return this

    val substitutedParameters = parameters.map { parameter =>
      val substitutedType = Type.substitute(parameter.tpe, assignments)
      parameter.copy(tpe = substitutedType)
    }
    val substitutedOutputType = Type.substitute(outputType, assignments)
    FunctionSignature(name, Vector.empty, substitutedParameters, substitutedOutputType, position)
  }
}

object FunctionSignature {
  def constant(name: NamePath, outputType: Type, position: Position): FunctionSignature = {
    FunctionSignature(name, Vector.empty, Vector.empty, outputType, position)
  }
}
