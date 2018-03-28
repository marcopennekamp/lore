package lore.exceptions

case class FunctionNotFoundException(name: String) extends RuntimeException(s"The function $name does not exist in the current scope.")
