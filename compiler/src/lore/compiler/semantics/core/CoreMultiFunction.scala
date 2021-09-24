package lore.compiler.semantics.core

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.Type

/**
  * A core multi-function is a multi-function that the compiler might directly invoke as a result of a transformation.
  * For example, non-primitive comparisons are transformed to core comparison function calls.
  *
  * @param outputType The expected output type of the multi-function.
  * @param mf The underlying multi-function. `None` if it cannot be resolved.
  */
class CoreMultiFunction(val name: NamePath, val outputType: Type, val mf: Option[MultiFunctionDefinition])
