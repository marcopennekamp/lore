package lore.utils

import scala.reflect.ClassTag

object CollectionExtensions {
  implicit class FilterTypeExtension[A](list: List[A]) {
    def filterType[T <: A](implicit tag: ClassTag[T]): List[T] = {
      list.flatMap {
        case value: T => Some(value)
        case _ => None
      }
    }
  }
}
