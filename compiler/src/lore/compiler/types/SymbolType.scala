package lore.compiler.types

import scala.util.hashing.MurmurHash3

case class SymbolType(name: String) extends Type {
  override val hashCode: Int = MurmurHash3.stringHash(name, 0xdc07cdd9)
}
