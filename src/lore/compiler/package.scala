package lore

import lore.ast.Node

package object compiler {
  /**
    * An abbreviation for Compilation[A].
    */
  type C[+A] = Compilation[A]

  /**
    * An abbreviation for Compilation[Unit]. A verification is an operation that returns nothing of note when
    * it is successful and fails with a set of errors if it isn't.
    */
  type Verification = C[Unit]

  /**
    * A wrapper for nodes to carry information of which fragment they originate from.
    */
  case class FragmentNode[+A <: Node](node: A, fragment: Fragment) {
    val position: Position = node.position(fragment)
  }
}
