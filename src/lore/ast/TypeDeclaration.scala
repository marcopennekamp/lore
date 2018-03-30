package lore.ast

case class TypeDeclaration(name: String, typeExpression: TypeExpression) extends TopLevelElement
