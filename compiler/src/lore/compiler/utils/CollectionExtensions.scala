package lore.compiler.utils

import scala.reflect.ClassTag

object CollectionExtensions {

  implicit class VectorExtension[A](vector: Vector[A]) {
    def filterType[T <: A](implicit tag: ClassTag[T]): Vector[T] = vector.flatMap {
      case value: T => Some(value)
      case _ => None
    }

    def filterNotType[T <: A](implicit tag: ClassTag[T]): Vector[A] = vector.flatMap {
      case _: T => None
      case value => Some(value)
    }
  }

  implicit class OptionExtension[A](option: Option[A]) {
    def filterType[T <: A](implicit tag: ClassTag[T]): Option[T] = option.flatMap {
      case value: T => Some(value)
      case _ => None
    }

    def filterNotType[T <: A](implicit tag: ClassTag[T]): Option[A] = option.flatMap {
      case _: T => None
      case value => Some(value)
    }
  }

}
