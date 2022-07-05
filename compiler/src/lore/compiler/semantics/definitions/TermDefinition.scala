package lore.compiler.semantics.definitions

import lore.compiler.semantics.bindings.TermBinding
import lore.compiler.semantics.modules.GlobalModule

/**
  * A user-defined term definition that can be used as a term binding.
  */
trait TermDefinition extends BindingDefinition with TermBinding {
  /**
    * A definition's companion module is a module defined alongside a definition with the same name. This currently
    * applies to the companion modules of struct <i>terms</i> (not struct types).
    */
  var companionModule: Option[GlobalModule] = None
}
