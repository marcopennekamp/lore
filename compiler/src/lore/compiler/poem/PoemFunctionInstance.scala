package lore.compiler.poem

import lore.compiler.semantics.NamePath

case class PoemFunctionInstance(name: NamePath, typeArguments: Vector[PoemType]) {
  override def toString: String = {
    if (typeArguments.isEmpty) name.toString
    else s"$name[${typeArguments.mkString(", ")}]"
  }
}
