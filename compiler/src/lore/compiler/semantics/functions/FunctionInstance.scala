package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.types.TypeVariable

/**
  * A [[FunctionDefinition]] with assigned type arguments and an instantiated signature. Hence, a function instance
  * represents a callable function whose type parameters are already instanced.
  */
case class FunctionInstance(
  definition: FunctionDefinition,
  assignments: TypeVariable.Assignments,
) extends Positioned {
  val signature: FunctionSignature = definition.signature.substitute(assignments)
  override def position: Position = definition.position
}
