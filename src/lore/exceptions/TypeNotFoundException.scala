package lore.exceptions

case class TypeNotFoundException(name: String) extends RuntimeException(s"The type $name does not exist in the current scope.")
