package lore.compiler.utils

import scala.reflect.ClassTag

object CollectionExtensions {
  // We could implement the filter type extensions generically using SeqOps, but it would complicate the API to
  // collection.filterType[A, Collection[A]], i.e. the programmer would have to respecify the That type because
  // we need the type parameter A.

  implicit class FilterTypeListExtension[A](list: List[A]) {
    def filterType[T <: A](implicit tag: ClassTag[T]): List[T] = filterTypeImpl[A, T](list).asInstanceOf[List[T]]
    def filterNotType[T <: A](implicit tag: ClassTag[T]): List[A] = filterNotTypeImpl[A, T](list).asInstanceOf[List[A]]
  }

  implicit class FilterTypeVectorExtension[A](vector: Vector[A]) {
    def filterType[T <: A](implicit tag: ClassTag[T]): Vector[T] = filterTypeImpl[A, T](vector).asInstanceOf[Vector[T]]
    def filterNotType[T <: A](implicit tag: ClassTag[T]): Vector[A] = filterNotTypeImpl[A, T](vector).asInstanceOf[Vector[A]]
  }

  implicit class FilterTypeOptionExtension[A](option: Option[A]) {
    def filterType[T <: A](implicit tag: ClassTag[T]): Option[T] = filterTypeImpl[A, T](option).asInstanceOf[Option[T]]
    def filterNotType[T <: A](implicit tag: ClassTag[T]): Option[A] = filterNotTypeImpl[A, T](option).asInstanceOf[Option[A]]
  }

  private def filterTypeImpl[A, T <: A](collection: IterableOnce[A])(implicit tag: ClassTag[T]): IterableOnce[T] = {
    collection.iterator.flatMap {
      case value: T => Some(value)
      case _ => None
    }
  }

  private def filterNotTypeImpl[A, T <: A](collection: IterableOnce[A])(implicit tag: ClassTag[T]): IterableOnce[A] = {
    collection.iterator.flatMap {
      case _: T => None
      case value => Some(value)
    }
  }
}
