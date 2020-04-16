package lore

import lore.ast.Node

package object compiler {
  /**
    * An abbreviation for Compilation[A].
    */
  type C[+A] = Compilation[A]

  /**
    * A wrapper for nodes to carry information of which fragment they originate from.
    */
  case class FragmentNode[+A <: Node](node: A, fragment: Fragment) {
    val position: Position = node.position(fragment)
  }
}
