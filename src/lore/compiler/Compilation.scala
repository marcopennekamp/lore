package lore.compiler

// TODO: How can we handle warnings?

/**
  * Represents a compilation to a value of type A. Either results in a Result[A] or an Errors object.
  *
  * We could use a Try[A], but we don't want to collect exceptions, rather [[CompilationError]]s. We could also use
  * an Either[List[CompilationError], A], but this is unwieldy and I'd like to have access to the implementation code
  * to add features later.
  */
sealed trait Compilation[+A] {
  /**
    * Continues the compilation in `fa` or aggregates further errors in `fb`.
    */
  def continue[B](fa: A => B, fb: List[CompilationError] => List[CompilationError]): Compilation[B] = this match {
    case Result(a) => Result(fa(a))
    case Errors(errors)  => Errors(fb(errors))
  }

  /**
    * Maps the result value of type A to a compilation resulting in B. In the context of compilations, flatMap is
    * to be understood as a "chain of computation" which is broken as soon as the first error appears.
    */
  def flatMap[B](f: A => Compilation[B]): Compilation[B] = this match {
    case Result(a) => f(a)
    case _ => this.asInstanceOf[Compilation[B]]
  }

  def map[B](f: A => B): Compilation[B] = flatMap(a => Result(f(a)))
}

case class Result[+A](value: A) extends Compilation[A]
case class Errors[+A](errors: List[CompilationError]) extends Compilation[A]

object Compilation {
  implicit class CompilationListExtension[A](compilations: List[Compilation[A]]) {
    /**
      * Combines all the compilations from a list into a single compilation. If any of the compilations have
      * resulted in an error, the combined compilation results in an error. This operation collects all errors
      * in unspecified order.
      */
    def combine: Compilation[List[A]] = {
      // We could also implement this using foldLeft, which would perhaps be more functional in style, but the
      // present definition is actually easier to parse, as foldLeft requires matching on two compilations for each
      // iteration.
      // This implementation, on the other hand, does not need to match on two compilations. We collect all values
      // and errors independently, then decide whether the combined compilation is a list of errors or results.
      // The decision is very simple: If the list of errors is not empty, there is at least one compilation that has
      // failed, and thus the whole compilation has failed with the given errors.
      var results = List.empty[A]
      var errors = List.empty[CompilationError]
      compilations.foreach {
        case Result(value) => results = value :: results
        case Errors(list) => errors = list ::: errors
      }
      if (errors.nonEmpty) Errors(errors) else Result(results)
    }
  }
}
