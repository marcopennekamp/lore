package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.C
import lore.compiler.phases.Phase
import lore.compiler.core.{Fragment, Registry}

class ResolutionPhase(fragments: List[Fragment]) extends Phase[Registry] {
  override lazy val result: C[Registry] = (new DeclarationResolver).buildRegistry(fragments)
}
