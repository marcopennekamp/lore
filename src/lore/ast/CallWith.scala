package lore.ast

case class CallWith(functionName: String, typeExpression: TypeExpression) extends TopLevelElement
