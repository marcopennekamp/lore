package lore.compiler.semantics

/**
  * NameKind signifies which kind of module member a simple name or name path should refer to.
  */
sealed trait NameKind

object NameKind {
  case object Type extends NameKind
  case object Binding extends NameKind
}
