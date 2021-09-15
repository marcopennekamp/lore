package lore.compiler.types

import lore.compiler.semantics.NamePath

trait NamedSchema extends TypeSchema {
  def name: NamePath
}
