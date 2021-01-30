package lore.compiler.semantics.functions

import lore.compiler.core.Positioned
import lore.compiler.types.Type

/**
  * The target of any kind of call. This is attached to nodes during verification so that calls are soundly typed
  * and can be properly resolved during transpilation.
  */
trait CallTarget {
  def name: String
  def outputType: Type
}

object CallTarget {
  /**
    * An internal call target is a Lore function or constructor. Hence, we can also expect a function signature.
    */
  trait Internal extends CallTarget with Positioned {
    def signature: FunctionSignature

    override def name: String = signature.name
    override def outputType: Type = signature.outputType
  }

  /**
    * A dynamic call target, meaning that we trust in the runtime to provide the correct bindings. We don't know
    * anything about the input type.
    */
  case class Dynamic(override val name: String, override val outputType: Type) extends CallTarget
}
