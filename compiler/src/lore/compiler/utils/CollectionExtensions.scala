package lore.compiler.utils

import lore.compiler.core.{ Compilation, Error }

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

    /**
      * Requires that the vector's elements are unique in respect to the key produced by the `key` function. If more
      * than one element share a key, the `duplicate` function is consulted with any representative element of that
      * group to produce a compilation error.
      */
    def requireUnique[K](key: A => K, duplicate: A => Error): Compilation[Vector[A]] = vector.groupBy(key).values.map {
      case Vector(a) => Compilation.succeed(a)
      case group if group.size > 1 => Compilation.fail(duplicate(group.head))
    }.toVector.simultaneous
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
