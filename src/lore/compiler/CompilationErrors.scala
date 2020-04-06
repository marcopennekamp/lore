package lore.compiler

object CompilationErrors {
  case class FunctionNotFound(name: String) extends RuntimeException(s"The function $name does not exist in the current scope.")
  case class TypeNotFound(name: String) extends RuntimeException(s"The type $name does not exist in the current scope.")
  case class ClassMustExtendClass(name: String) extends RuntimeException(s"The class $name does not extend a class but some other type.")
  case class LabelMustExtendLabel(name: String) extends RuntimeException(s"The label $name does not extend a label but some other type.")
}
