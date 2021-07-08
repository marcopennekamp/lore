package lore.compiler.utils

import lore.compiler.core.Compilation
import lore.compiler.feedback.Feedback

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
      * Returns distinct elements based on the comparison function `areEqual`. This is in contrast to the standard
      * library functions `distinct` and `distinctBy`, which only operate using standard `==` equality.
      */
    def distinctUsing(areEqual: (A, A) => Boolean): Vector[A] = vector.foldLeft(Vector.empty[A]) {
      (result, a) => if (!result.exists(areEqual(a, _))) result :+ a else result
    }

    /**
      * Ensures that all elements in the vector are equal to each other.
      */
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

    /**
      * Returns this vector or, if this vector is empty, a vector containing the given element.
      */
    def withDefault[B >: A](default: => B): Vector[B] = if (vector.nonEmpty) vector else Vector(default)
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
