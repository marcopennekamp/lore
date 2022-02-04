package lore.compiler.assembly

import lore.compiler.poem.Poem
import lore.compiler.semantics.scopes.LocalVariable

package object expressions {
  /**
    * This maps uniqueKeys of LocalVariables to a unique register ID. Unique keys are necessary so that local variables
    * defined in different scopes aren't accidentally conflated.
    */
  type VariableRegisterMap = Map[LocalVariable.UniqueKey, Poem.Register]
}
