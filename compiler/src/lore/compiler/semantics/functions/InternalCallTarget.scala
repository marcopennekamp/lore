package lore.compiler.semantics.functions

import lore.compiler.core.Positioned
import lore.compiler.types.Type

// TODO: Move to the companion object of CallTarget as CallTarget.Internal?

/**
  * An internal call target is a Lore function or constructor. Hence, we can also expect a function signature.
  */
trait InternalCallTarget extends CallTarget with Positioned {
  def signature: FunctionSignature

  override def name: String = signature.name
  override def outputType: Type = signature.outputType
}
