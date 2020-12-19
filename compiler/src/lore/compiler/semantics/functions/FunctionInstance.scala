package lore.compiler.semantics.functions

import lore.compiler.core.Position

/**
  * A [[FunctionDefinition]] with a signature that has all of its type variables replaced with their instances.
  * Hence, a function instance represents a callable function whose type parameters are already instanced.
  */
case class FunctionInstance(
  definition: FunctionDefinition,
  override val signature: FunctionSignature
) extends InternalCallTarget {
  override def position: Position = definition.position
}
