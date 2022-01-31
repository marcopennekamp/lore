package lore.compiler.poem

abstract class PoemValue

case class PoemIntValue(value: Long) extends PoemValue
case class PoemRealValue(value: Double) extends PoemValue
case class PoemBooleanValue(value: Boolean) extends PoemValue
case class PoemStringValue(value: String) extends PoemValue
