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

    /**
      * Lifts the vector's fold operation into an Option context. The fold uses [[Option.flatMap]], so it continues
      * until None is encountered.
      */
    def foldSome[B](initial: B)(f: (B, A) => Option[B]): Option[B] = {
      vector.foldLeft(Some(initial): Option[B]) { case (option, element) => option.flatMap(f(_, element)) }
    }
  }

  implicit class SetExtension[A](set: Set[A]) {
    def filterType[T <: A](implicit tag: ClassTag[T]): Set[T] = set.flatMap {
      case value: T => Some(value)
      case _ => None
    }

    /**
      * Returns this set or, if this set is empty, a set containing the given element.
      */
    def withDefaultSingle(default: => A): Set[A] = if (set.nonEmpty) set else Set(default)

    /**
      * Returns this set or, if this set is empty, a default set.
      */
    def withDefault(default: => Set[A]): Set[A] = if (set.nonEmpty) set else default
  }

  implicit class MapVectorExtension[K, V](vector: Vector[Map[K, V]]) {
    /**
      * Merges all maps in `vector` such that map values occurring at same key are merged into a vector.
      */
    def merged: Map[K, Vector[V]] = {
      vector.foldLeft(Map.empty[K, Vector[V]]) { case (result, map) =>
        map.foldLeft(result) {
          case (result, (key, value)) => result.appended(key, value)
        }
      }
    }
  }

  implicit class VectorMapExtension[K, V](map: Map[K, Vector[V]]) {
    /**
      * Appends `value` to the vector at `key`.
      */
    def appended(key: K, value: V): Map[K, Vector[V]] = {
      map.updated(
        key,
        map.get(key) match {
          case Some(vector) => vector :+ value
          case None => Vector(value)
        },
      )
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

    /**
      * Invokes `f` if the option is a None. Returns the option for chaining.
      */
    def ifEmpty(f: => Unit): Option[A] = {
      if (option.isEmpty) f
      option
    }
  }

  implicit class OptionVectorExtension[A](vector: Vector[Option[A]]) {
    /**
      * If all options in this vector are defined, returns some vector containing all option values. Otherwise, if at
      * least one option is None, returns None.
      */
    def sequence: Option[Vector[A]] = if (vector.exists(_.isEmpty)) None else Some(vector.map(_.get))
  }

  implicit class OptionTuple2Extension[A, B](tuple: (Option[A], Option[B])) {
    /**
      * If all options in this tuple are defined, returns some tuple containing all option values. Otherwise, if at
      * least one option is None, returns None.
      */
    def sequence: Option[(A, B)] = {
      val (a, b) = tuple
      if (a.isEmpty) None
      else if (b.isEmpty) None
      else Some(a.get, b.get)
    }
  }

}
