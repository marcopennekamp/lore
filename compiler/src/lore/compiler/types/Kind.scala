package lore.compiler.types

sealed trait Kind

object Kind {
  case object TypeVariable extends Kind
  case object Any extends Kind
  case object Nothing extends Kind
  case object Int extends Kind
  case object Real extends Kind
  case object Boolean extends Kind
  case object String extends Kind
  case object Symbol extends Kind
  case object Sum extends Kind
  case object Intersection extends Kind
  case object Tuple extends Kind
  case object Function extends Kind
  case object List extends Kind
  case object Map extends Kind
  case object Shape extends Kind
  case object Trait extends Kind
  case object Struct extends Kind
}
