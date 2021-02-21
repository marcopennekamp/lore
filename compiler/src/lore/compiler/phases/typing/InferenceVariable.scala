package lore.compiler.phases.typing

import lore.compiler.phases.typing.InferenceVariable.nameCounter
import lore.compiler.types.Type

/**
  * Inference variables are strictly reference-equal, much like type variables. They can be put in types in place of
  * other types and are resolved by the type inference algorithm.
  */
class InferenceVariable(val name: Option[String] = None) extends Type {
  override def equals(obj: Any): Boolean = obj match {
    case var2: InferenceVariable => this eq var2
    case _ => false
  }

  // TODO: This is only temporary!!
  lazy val actualName: String = name.getOrElse {
    nameCounter += 1
    s"iv$nameCounter"
  }

  override def toString: String = actualName
}

object InferenceVariable {
  // TODO: This is only temporary!!
  protected var nameCounter = 0
}
