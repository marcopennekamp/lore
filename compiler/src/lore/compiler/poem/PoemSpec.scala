package lore.compiler.poem

import lore.compiler.semantics.NamePath

case class PoemSpec(
  name: NamePath,
  isTest: Boolean,
  isBenchmark: Boolean,
  executableName: NamePath,
)
