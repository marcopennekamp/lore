package lore.compiler.semantics.scopes

import lore.compiler.target.TargetIdentifiable

trait Variable extends TargetIdentifiable {
  def name: String
  def isMutable: Boolean = false
}
