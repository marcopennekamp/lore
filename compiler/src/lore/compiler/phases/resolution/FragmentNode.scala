package lore.compiler.phases.resolution

import lore.compiler.ast.Node
import lore.compiler.Fragment
import lore.compiler.feedback.Position

/**
  * A wrapper for nodes to carry information of which fragment they originate from.
  */
case class FragmentNode[+A <: Node](node: A, fragment: Fragment) {
  val position: Position = node.position(fragment)
}
