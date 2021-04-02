package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}

/**
  * A [[FunctionDefinition]] with a signature that has all of its type variables replaced with their instances. Hence,
  * a function instance represents a callable function whose type parameters are already instanced.
  */
case class FunctionInstance(
  definition: FunctionDefinition,
  signature: FunctionSignature,
) extends Positioned {
  override def position: Position = definition.position
}
