package lore.compiler.semantics.definitions

import lore.compiler.core.Positioned

/**
  * A user-defined type, term, or spec. Struct <i>terms</i> are not considered definitions.
  */
trait Definition extends Positioned
