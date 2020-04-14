package lore.compiler

import lore.ast.{DeclNode, Index}

class Fragment(val declarations: List[DeclNode])

object Fragment {
  /**
    * A fragment position identifies a code location across a whole Lore project.
    */
  case class Position(fragment: Fragment, index: Index)
}
