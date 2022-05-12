package lore.compiler.semantics

import lore.compiler.core.Positioned

trait Definition extends Positioned {
  def name: NamePath
}
