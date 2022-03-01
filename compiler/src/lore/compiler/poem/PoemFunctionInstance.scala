package lore.compiler.poem

import lore.compiler.semantics.NamePath

case class PoemFunctionInstance(name: NamePath, typeArguments: Vector[PoemType]) {
  override def toString: String = s"$name[${typeArguments.mkString(", ")}]"
}
