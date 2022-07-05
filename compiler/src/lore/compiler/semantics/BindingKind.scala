package lore.compiler.semantics

// TODO (multi-import): Rename to DefinitionKind and move to `definitions` package.

/**
  * [[BindingKind]] signifies which kind of binding a module member or name path should refer to.
  */
sealed trait BindingKind {
  /**
    * Whether bindings of this kind may be defined multiple times across multiple different local modules.
    */
  def isMultiDefinable: Boolean = this == BindingKind.Module || this == BindingKind.MultiFunction

  /**
    * Whether multiple distinct bindings of this kind may be referred to via a single simple name, which will induce
    * the need for disambiguation at compile time in some way before one of the bindings can be used.
    *
    * Only multi-functions are currently multi-referable, but the implementation is applicable generally.
    */
  def isMultiReferable: Boolean = this == BindingKind.MultiFunction
  def isSingleReferable: Boolean = !isMultiReferable
}

object BindingKind {
  case object Type extends BindingKind

  sealed trait Term extends BindingKind
  case object Module extends Term
  case object Struct extends Term
  case object GlobalVariable extends Term
  case object MultiFunction extends Term
}
