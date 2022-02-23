package lore.compiler.poem

import lore.compiler.semantics.NamePath

sealed trait PoemGlobalVariable {
  def name: NamePath
}

case class PoemEagerGlobalVariable(name: NamePath, value: PoemValue) extends PoemGlobalVariable
case class PoemLazyGlobalVariable(name: NamePath, initializerName: NamePath) extends PoemGlobalVariable
