package lore.definitions

import lore.compiler.C
import lore.types.Type

class ParameterDefinition(val name: String, override val resolveType: () => C[Type]) extends TypingDeferred[Type] {
  override def toString = s"$name: $tpe"
}
