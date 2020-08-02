package lore.compiler.semantics.functions

import lore.compiler.types.Type

// TODO: Move to the companion object of CallTarget as CallTarget.Dynamic?

/**
  * A dynamic call target, meaning that we trust in the runtime to provide the correct bindings. We don't know
  * anything about the input type.
  */
case class DynamicCallTarget(override val name: String, override val outputType: Type) extends CallTarget
