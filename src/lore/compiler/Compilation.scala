package lore.compiler

// TODO: Performance-wise, it might be worth to use doubly-linked lists here, if performance ever becomes a problem.

/**
  * Represents a compilation to a value of type A. Either results in a Result[A] or an Errors object.
  *
  * We could use a Try[A], but we don't want to collect exceptions, rather [[Feedback]]s. We could also use
  * an Either[List[CompilationError], A], but this is unwieldy and I'd like to have access to the implementation code
  * to add features later.
  */
sealed trait Compilation[+A] {
  /**
    * A list of info feedback that is always carried forward through operations such as flatMap and combine.
    */
  def infos: List[InfoFeedback]

  /**
    * Continues the compilation in `fa` or aggregates further errors in `fb`.
    */
  def continue[B](fa: A => B, fb: List[Error] => List[Error]): Compilation[B] = this match {
    case Result(a, infos) => Result(fa(a), infos)
    case Errors(errors, infos)  => Errors(fb(errors), infos)
  }

  /**
    * Maps the result value of type A to a compilation resulting in B. In the context of compilations, flatMap is
    * to be understood as a "chain of computation" which is broken as soon as the first error appears.
    */
  def flatMap[B](f: A => Compilation[B]): Compilation[B] = this match {
    case Result(a, infos) => f(a).withInfos(infos)
    case _ => this.asInstanceOf[Compilation[B]]
  }

  def map[B](f: A => B): Compilation[B] = flatMap(a => Result(f(a), List.empty))

  /**
    * Attaches the given list of infos to a copy of the current compilation.
    */
  def withInfos(infos: List[InfoFeedback]): Compilation[A] = this match {
    case Result(a, infos2) => Result(a, infos ++ infos2)
    case Errors(errors, infos2) => Errors(errors, infos ++ infos2)
  }
}

case class Result[+A](value: A, override val infos: List[InfoFeedback]) extends Compilation[A]
case class Errors[+A](errors: List[Error], override val infos: List[InfoFeedback]) extends Compilation[A]

object Compilation {
  def fail(errors: Error*): Compilation[Nothing] = Errors(errors.toList, List.empty)
  def failInfo(errors: Error*)(infos: InfoFeedback*): Compilation[Nothing] = Errors(errors.toList, infos.toList)
  def succeed[A](a: A): Compilation[A] = Result(a, List.empty)
  def succeedInfo[A](a: A)(infos: InfoFeedback*): Compilation[A] = Result(a, infos.toList)

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
      var errors = List.empty[Error]
      var infos = List.empty[InfoFeedback]
      compilations.foreach {
        case Result(value, infos2) =>
          results = value :: results
          infos = infos2 ::: infos
        case Errors(errors2, infos2) =>
          errors = errors2 ::: errors
          infos = infos2 ::: infos
      }
      if (errors.nonEmpty) Errors(errors, infos) else Result(results, infos)
    }
  }
}
