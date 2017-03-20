package lore.parser

trait ExpressionParser[T] {
  def parseExpression(text: String): T
}
