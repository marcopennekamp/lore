package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.ParameterDefinition.NamedParameterView
import lore.compiler.types._

import scala.util.hashing.MurmurHash3

case class FunctionSignature(
  name: NamePath,
  typeParameters: Vector[TypeVariable],
  parameters: Vector[ParameterDefinition],
  outputType: Type,
  position: Position,
) extends Positioned {
  val namedParameters: Vector[NamedParameterView] = parameters.filter(_.name.isDefined).map(NamedParameterView)
  val inputType: TupleType = TupleType(parameters.map(_.tpe))
  val isPolymorphic: Boolean = typeParameters.nonEmpty
  val isMonomorphic: Boolean = !isPolymorphic
  val arity: Int = parameters.size
  val functionType: FunctionType = FunctionType(inputType, outputType)
  override def toString: String = s"$name$inputType: $outputType"
  override val hashCode: Int = MurmurHash3.productHash((name, inputType, outputType))

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
