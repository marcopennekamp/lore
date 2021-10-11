package lore.compiler.types

sealed trait Kind

object Kind {
  case object TypeVariable extends Kind
  case object BasicType extends Kind
  case object Sum extends Kind
  case object Intersection extends Kind
  case object Tuple extends Kind
  case object Function extends Kind
  case object List extends Kind
  case object Map extends Kind
  case object Shape extends Kind
  case object Symbol extends Kind
  case object DeclaredType extends Kind
}
