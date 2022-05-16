package lore.compiler.poem

import lore.compiler.semantics.NamePath

case class PoemSpec(
  moduleName: NamePath,
  description: String,
  isTest: Boolean,
  isBenchmark: Boolean,
  executableName: NamePath,
)
