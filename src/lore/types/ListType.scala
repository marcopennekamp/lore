package lore.types

case class ListType(element: Type) extends Type {
  /**
    * Lists are always concrete as the empty list exists for all element types.
    */
  override def isAbstract = false

  override def string(parentPrecedence: TypePrecedence): String = s"[${element.string(TypePrecedence.Parenthesized)}]"
}
