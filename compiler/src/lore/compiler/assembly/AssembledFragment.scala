package lore.compiler.assembly

import java.nio.file.Path

case class AssembledFragment(path: Path, bytecode: Array[Byte])
