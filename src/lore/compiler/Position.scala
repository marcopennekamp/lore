package lore.compiler

import lore.ast.Index

/**
  * A position identifies a code location across a whole Lore project.
  */
case class Position(fragment: Fragment, index: Index)
