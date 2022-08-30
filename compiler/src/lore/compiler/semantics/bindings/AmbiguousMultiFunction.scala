package lore.compiler.semantics.bindings

import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.modules.MultiReference

/**
  * A multi-function binding backed by a [[MultiReference]]. This needs to be disambiguated.
  *
  * TODO (multi-import): Is this still the correct approach or should we handle multi-reference bindings differently?
  */
case class AmbiguousMultiFunction(mfs: MultiReference[MultiFunctionDefinition]) extends TermBinding
