package lore.compiler.utils

import scala.reflect.ClassTag

object CollectionExtensions {
  implicit class FilterTypeExtension[A](sequence: Seq[A]) {
    def filterType[T <: A](implicit tag: ClassTag[T]): Seq[T] = {
      sequence.flatMap {
        case value: T => Some(value)
        case _ => None
      }
    }
    def filterNotType[T <: A](implicit tag: ClassTag[T]): Seq[A] = {
      sequence.flatMap {
        case _: T => None
        case value => Some(value)
      }
    }
  }
}
