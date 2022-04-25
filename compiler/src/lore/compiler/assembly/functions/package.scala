package lore.compiler.assembly

import lore.compiler.core.UniqueKey
import lore.compiler.poem.Poem

package object functions {
  /**
    * This maps unique keys to a unique register ID. Unique keys are necessary so that local variables defined in
    * different scopes aren't accidentally conflated.
    */
  type VariableRegisterMap = Map[UniqueKey, Poem.Register]

  /**
    * Assigns a unique index to the unique key of each captured variable. The index must correspond to the position of
    * the register in the `FunctionLambda` instruction and will be used by `LambdaLocal` to load the value.
    */
  type CapturedVariableMap = Map[UniqueKey, Int]
}
