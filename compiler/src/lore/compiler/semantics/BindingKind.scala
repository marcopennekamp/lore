package lore.compiler.semantics

/**
  * BindingKind signifies which kind of binding a simple name or name path should refer to.
  */
sealed trait BindingKind

object BindingKind {
  case object Type extends BindingKind
  case object Term extends BindingKind
}
