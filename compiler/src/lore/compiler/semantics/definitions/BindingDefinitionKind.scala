package lore.compiler.semantics.definitions

/**
  * [[BindingDefinitionKind]] signifies the kind of a binding definition, i.e. whether the definition is
  * multi-definable and/or multi-referable.
  */
sealed trait BindingDefinitionKind {
  /**
    * Whether bindings of this kind may be defined multiple times across multiple different local modules.
    */
  def isMultiDefinable: Boolean = this == BindingDefinitionKind.Module || this == BindingDefinitionKind.MultiFunction

  /**
    * Whether multiple distinct bindings of this kind may be referred to via a single simple name, which will induce
    * the need for disambiguation at compile time in some way before one of the bindings can be used.
    *
    * Only multi-functions are currently multi-referable, but the implementation is applicable generally.
    */
  def isMultiReferable: Boolean = this == BindingDefinitionKind.MultiFunction
  def isSingleReferable: Boolean = !isMultiReferable
}

object BindingDefinitionKind {
  case object Type extends BindingDefinitionKind

  sealed trait Term extends BindingDefinitionKind
  case object Module extends Term
  case object Struct extends Term
  case object GlobalVariable extends Term
  case object MultiFunction extends Term
}
