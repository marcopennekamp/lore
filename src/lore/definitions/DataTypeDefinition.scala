package lore.definitions

trait DataTypeDefinition extends DeclaredTypeDefinition {
  /**
    * The list of all members belonging to this data type, including superclass members.
    */
  def members: List[MemberDefinition]

  /**
    * The list of all properties belonging to this data type, including superclass properties.
    */
  def properties: List[PropertyDefinition]
}
