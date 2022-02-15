package lore.compiler.assembly

import lore.compiler.core.UniqueKey
import lore.compiler.poem.Poem

package object expressions {
  /**
    * This maps unique keys to a unique register ID. Unique keys are necessary so that local variables defined in
    * different scopes aren't accidentally conflated.
    */
  type VariableRegisterMap = Map[UniqueKey, Poem.Register]
}
