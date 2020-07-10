package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.C
import lore.compiler.core.{Fragment, Registry}
import lore.compiler.phases.Phase

class ResolutionPhase(fragments: List[Fragment]) extends Phase[Registry] {
  override lazy val result: C[Registry] = (new DeclarationResolver).buildRegistry(fragments)
}
