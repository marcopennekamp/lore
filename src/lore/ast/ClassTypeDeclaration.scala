package lore.ast

case class ClassTypeDeclaration(name: String, supertypeName: Option[String], isAbstract: Boolean) extends TopLevelElement
