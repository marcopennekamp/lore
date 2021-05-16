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

    def separateByType[T <: A](implicit tag: ClassTag[T]): (Vector[A], Vector[T]) = {
      var as = Vector.empty[A]
      var ts = Vector.empty[T]
      vector.foreach {
        case t: T => ts = ts :+ t
        case a => as = as :+ a
      }
      (as, ts)
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

    def allEqual[V](by: A => V): Boolean = {
      vector.length <= 1 || vector.sliding(2).forall { case Vector(left, right) => by(left) == by(right) }
    }

    /**
      * Applies the given function to each element in the list, returning immediately if a defined result has been
      * returned by a call `f(a)`.
      */
    def firstDefined[B](f: A => Option[B]): Option[B] = {
      for (a <- vector) {
        f(a) match {
          case some@Some(_) => return some
          case None =>
        }
      }
      None
    }
  }

  implicit class SetExtension[A](set: Set[A]) {
    def filterType[T <: A](implicit tag: ClassTag[T]): Set[T] = set.flatMap {
      case value: T => Some(value)
      case _ => None
    }

    def ifEmptySingle(v: => A): Set[A] = if (set.nonEmpty) set else Set(v)

    def ifEmpty(set2: => Set[A]): Set[A] = if (set.nonEmpty) set else set2
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
