package lore.compiler

// TODO: Find a better way to accumulate errors (maybe with a monad) and report all errors at once, instead of
//       terminating after finding only one error. This applies to type checks, constraint checks, illegal expressions,
//       illegal declarations, and so on.
object CompilationErrors {
  case class FunctionNotFound(name: String) extends RuntimeException(s"The function $name does not exist in the current scope.")
  case class TypeNotFound(name: String) extends RuntimeException(s"The type $name does not exist in the current scope.")
  case class ClassMustExtendClass(name: String) extends RuntimeException(s"The class $name does not extend a class but some other type.")
  case class LabelMustExtendLabel(name: String) extends RuntimeException(s"The label $name does not extend a label but some other type.")
  case class ComponentTypeMustContainClass(name: String) extends RuntimeException(s"The component type +$name is not valid since $name is not a class.")
}
