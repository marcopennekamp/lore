package lore.definitions

trait DataTypeDefinition extends DeclaredTypeDefinition {
  def properties: List[PropertyDefinition]
}
