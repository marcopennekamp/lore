package lore.compiler.utils

import scala.reflect.ClassTag

object CollectionExtensions {
  implicit class FilterTypeExtension[A](list: List[A]) {
    def filterType[T <: A](implicit tag: ClassTag[T]): List[T] = {
      list.flatMap {
        case value: T => Some(value)
        case _ => None
      }
    }
    def filterNotType[T <: A](implicit tag: ClassTag[T]): List[A] = {
      list.flatMap {
        case _: T => None
        case value => Some(value)
      }
    }
  }
}
