package lore.compiler.types

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.bindings.Binding

/**
  * A named schema is synonymous with [[lore.compiler.semantics.bindings.TypeBinding]].
  */
trait NamedSchema extends TypeSchema with Binding {
  def name: NamePath
}
