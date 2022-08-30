package lore.compiler.utils

import scala.reflect.ClassTag

object CollectionExtensions {

  implicit class IterableExtension[A](iterable: Iterable[A]) {
    def filterType[T](implicit tag: ClassTag[T]): Iterable[T] = iterable.filter {
      case _: T => true
      case _ => false
    }.asInstanceOf[Iterable[T]]
  }

  implicit class VectorExtension[A](vector: Vector[A]) {
    def filterType[T](implicit tag: ClassTag[T]): Vector[T] = vector.foldLeft(Vector.empty[T]) {
      case (result, value: T) => result :+ value
      case (result, _) => result
    }

    def filterNotType[T <: A](implicit tag: ClassTag[T]): Vector[A] = vector.foldLeft(Vector.empty[A]) {
      case (result, _: T) => result
      case (result, value) => result :+ value
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
      * Lifts the vector's fold operation into an Option context. The fold continues until None is encountered.
      */
    def foldSome[B](initial: B)(f: (B, A) => Option[B]): Option[B] = {
      vector.foldLeft(Some(initial): Option[B]) {
        case (Some(value), element) => f(value, element)
        case (None, _) => return None
      }
    }

    /**
      * A specialized [[foldSome]] that expects `f` to produce one `B` value per `A` value, which are then collected in
      * a result vector, in addition to the accumulator.
      */
    def foldSomeCollect[B, C](initial: C)(f: (C, A) => Option[(B, C)]): Option[(Vector[B], C)] = {
      vector.foldSome((Vector.empty[B], initial)) {
        case ((result, acc), element) => f(acc, element).map { case (value, acc2) => (result :+ value, acc2) }
      }
    }

    /**
      * Checks whether the vector is sorted.
      */
    def isSorted(implicit ev: A => Ordered[A]): Boolean = {
      if (vector.length < 2) true
      else vector.sliding(2).forall { case Vector(a, b) => a < b }
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

  implicit class MapExtension[K, V](map: Map[K, V]) {
    /**
      * Inverts the map, turning keys into values and values into keys.
      */
    def invert: Map[V, Vector[K]] = {
      map.foldLeft(Map.empty[V, Vector[K]]) {
        case (result, (key, value)) => result.appended(value, key)
      }
    }
  }

  implicit class MapVectorExtension[K, V](vector: Vector[Map[K, V]]) {
    /**
      * Merges all maps in `vector` such that map values occurring at the same key are merged into a vector.
      */
    def merged: Map[K, Vector[V]] = {
      vector.foldLeft(Map.empty[K, Vector[V]]) { case (result, map) =>
        map.foldLeft(result) {
          case (result, (key, value)) => result.appended(key, value)
        }
      }
    }
  }

  implicit class MapTuple2Extension[K, V1, V2](tuple: (Map[K, V1], Map[K, V2])) {
    /**
      * Merges the maps in `tuple` such that map values occurring at same key are merged into a tuple. Missing values
      * are represented by `None`.
      */
    def merged: Map[K, (Option[V1], Option[V2])] = {
      val (map1, map2) = tuple
      val keys = (map1.keys.toVector ++ map2.keys).distinct
      keys.map(key => key -> (map1.get(key), map2.get(key))).toMap
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
    def filterType[T](implicit tag: ClassTag[T]): Option[T] = option.flatMap {
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

  implicit class Tuple2Extension[A, B](tuple: (A, B)) {
    def mapFirst[R](f: A => R): (R, B) = (f(tuple._1), tuple._2)
  }

  implicit class Tuple3Extension[A, B, C](tuple: (A, B, C)) {
    def mapFirst[R](f: A => R): (R, B, C) = (f(tuple._1), tuple._2, tuple._3)
  }

  implicit class Tuple2OptionExtension[A, B](option: Option[(A, B)]) {
    def flatMapFirst[R](f: A => Option[R]): Option[(R, B)] = option.flatMap { case (a, b) => f(a).map((_, b)) }
    def mapFirst[R](f: A => R): Option[(R, B)] = option.map { case (a, b) => (f(a), b) }
  }

  implicit class Tuple3OptionExtension[A, B, C](option: Option[(A, B, C)]) {
    def flatMapFirst[R](f: A => Option[R]): Option[(R, B, C)] = option.flatMap { case (a, b, c) => f(a).map((_, b, c)) }
    def mapFirst[R](f: A => R): Option[(R, B, C)] = option.map { case (a, b, c) => (f(a), b, c) }
  }

}
