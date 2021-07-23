package lore.compiler.types

trait NamedSchema extends TypeSchema {
  def name: String
  override def toString: String = name
}
