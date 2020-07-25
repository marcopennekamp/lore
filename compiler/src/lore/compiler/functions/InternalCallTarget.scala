package lore.compiler.functions

import lore.compiler.core.feedback.Positioned
import lore.compiler.types.Type

/**
  * An internal call target is a Lore function or constructor. Hence, we can also expect a function signature.
  */
trait InternalCallTarget extends CallTarget with Positioned {
  def signature: FunctionSignature

  override def name: String = signature.name
  override def outputType: Type = signature.outputType
}
