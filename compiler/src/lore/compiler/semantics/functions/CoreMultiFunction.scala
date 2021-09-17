package lore.compiler.semantics.functions

import lore.compiler.semantics.NamePath
import lore.compiler.types.{TupleType, Type}

/**
  * A core multi-function is a multi-function that the compiler might directly invoke as a result of a transformation.
  * For example, non-primitive comparisons are transformed to core comparison function calls.
  */
case class CoreMultiFunction(name: NamePath, inputType: TupleType, outputType: Type) {
  override def equals(obj: Any): Boolean = obj match {
    case other: CoreMultiFunction => name == other.name
    case _ => false
  }
  override val hashCode: Int = name.hashCode()
}
