package lore.compiler.phases.parsing

import lore.compiler.core.Fragment
import lore.compiler.feedback.Reporter
import lore.compiler.syntax.DeclNode

object ParsingPhase {

  /**
    * Parses all fragments, resulting in a flattened list of declaration nodes.
    */
  def process(fragments: Vector[Fragment])(implicit reporter: Reporter): Vector[DeclNode] = {
    fragments.flatMap(fragment => new FragmentParser()(fragment).parse())
  }

}
