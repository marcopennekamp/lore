package lore.compiler.functions

import lore.compiler.feedback.Positioned
import lore.compiler.types.Type

trait InternalCallTarget extends CallTarget with Positioned {
  def signature: FunctionSignature

  override def name: String = signature.name
  override def outputType: Type = signature.outputType
}
