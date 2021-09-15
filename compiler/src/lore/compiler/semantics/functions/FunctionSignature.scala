package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.ParameterDefinition.NamedParameterDefinition
import lore.compiler.types._

import scala.util.hashing.MurmurHash3

case class FunctionSignature(
  name: NamePath,
  parameters: Vector[ParameterDefinition],
  outputType: Type,
  position: Position,
) extends Positioned {
  val namedParameters: Vector[NamedParameterDefinition] = parameters.filter(_.name.isDefined).map(NamedParameterDefinition)
  val inputType: TupleType = TupleType(parameters.map(_.tpe))
  val isPolymorphic: Boolean = Type.isPolymorphic(inputType)
  val isMonomorphic: Boolean = !isPolymorphic
  val arity: Int = parameters.size
  val functionType: FunctionType = FunctionType(inputType, outputType)
  override def toString: String = s"$name$inputType: $outputType"
  override val hashCode: Int = MurmurHash3.productHash((name, inputType, outputType))

  /**
    * Substitutes the given type variable assignments into the parameter types and output type and returns
    * a new function signature.
    */
  def substitute(assignments: TypeVariable.Assignments): FunctionSignature = {
    if (assignments.isEmpty) return this

    val substitutedParameters = parameters.map { parameter =>
      val substitutedType = Type.substitute(parameter.tpe, assignments)
      ParameterDefinition(parameter.name, substitutedType, parameter.position)
    }
    val substitutedOutputType = Type.substitute(outputType, assignments)
    FunctionSignature(name, substitutedParameters, substitutedOutputType, position)
  }
}
