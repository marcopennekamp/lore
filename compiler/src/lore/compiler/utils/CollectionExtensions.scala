package lore.compiler.utils

import scala.reflect.ClassTag

object CollectionExtensions {

  implicit class VectorExtension[A](vector: Vector[A]) {
    /**
      * Correlates two vectors with the given predicate, returning the pairs that match, and the lists of As and Bs
      * that don't have a partner.
      */
    def correlate[B](bs: Vector[B])(predicate: (A, B) => Boolean): (Vector[(A, B)], Vector[A], Vector[B]) = {
      var pairs = Vector.empty[(A, B)]
      for (a <- vector; b <- bs) {
        if (predicate(a, b)) pairs = pairs :+ (a, b)
      }

      val leftSingles = vector.diff(pairs.map(_._1))
      val rightSingles = bs.diff(pairs.map(_._2))

      (pairs, leftSingles, rightSingles)
    }

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
