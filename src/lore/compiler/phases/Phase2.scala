package lore.compiler.phases

import lore.compiler.{C, DeclarationResolver, Fragment, Registry}

class Phase2(fragments: List[Fragment]) extends Phase[Registry] {
  override lazy val result: C[Registry] = (new DeclarationResolver).buildRegistry(fragments)
}
