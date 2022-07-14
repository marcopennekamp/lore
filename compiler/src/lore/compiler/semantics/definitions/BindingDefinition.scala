package lore.compiler.semantics.definitions

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.bindings.Binding

/**
  * A named type or term that can be used as a binding.
  *
  * Most binding definitions are built in a two-phase approach:
  *   1. The definition itself is <b>created</b> so that it can be the result of name resolution and cross-referenced
  *      by other definitions.
  *   2. At some later point, the definition is <b>initialized</b>, resolving all parts of the definition which depend
  *      on other definitions that might have been created after this definition.
  *
  * This requires definitions to be partially mutable, but a purely immutable solution leads to complicated workarounds
  * due to the innate interdependency of definitions. Some especially tricky parts are:
  *   - If we don't immediately register all definitions in a global module, we need to pre-register some intermediate
  *     structure (e.g. a `ModuleMember`). This allows local modules to know about all their members to resolve names,
  *     and take into account imports and multi-referable definitions, <i>while</i> other definitions are being
  *     resolved. Pre-creating all definitions allows us to avoid these intermediate structures, simplifying the
  *     architectural burden considerably.
  *   - A struct that's earlier in the schema resolution order might access the companion module of a struct that's
  *     later in the order (see `test/language/modules/companion_cycle.lore`). But if the struct term hasn't been
  *     resolved yet, it cannot be returned from a scope. This edge case requires a workaround in term scopes that
  *     handles the possibility of an unresolved struct term, returning the companion module instead. Pre-creating all
  *     definitions removes the need for this workaround.
  */
trait BindingDefinition extends Definition with Binding {
  def name: NamePath
  def definitionKind: BindingDefinitionKind
  def isInitialized: Boolean

  def simpleName: String = name.simpleName
}
