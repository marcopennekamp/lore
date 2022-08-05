package lore.compiler.semantics.bindings

import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.modules.MultiReference

/**
  * A multi-function binding backed by a [[MultiReference]]. This needs to be disambiguated.
  */
case class AmbiguousMultiFunction(multiReference: MultiReference[MultiFunctionDefinition]) extends TermBinding
