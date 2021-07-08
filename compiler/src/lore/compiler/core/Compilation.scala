package lore.compiler.core

import lore.compiler.core.Compilation.{EmptyFailure, Failure, Result, ResultFailure, Success, Verification}
import lore.compiler.feedback.Feedback

/**
  * Represents a compilation to a result of type A. Compilations are defined in two dimensions: (1) whether a result
  * was produced, and (2) whether the compilation was successful or a failure. Compilations without a result are called
  * empty. Empty compilations are always failures.
  *
  * The point of separating these two notions is simple to understand. When we come upon an error in a particular
  * phase, say during resolution we find that a single struct definition extends a non-existent type, we don't want to
  * abort compilation at this stage. We have a multitude of valid definitions already resolved that could be further
  * verified and checked. There is a good chance that a Registry can be produced, with only the particular extended
  * type erased from the incorrect struct definition. This is not very important when the code is compiled from the
  * command line. The programmer simply fixes all errors phase-by-phase. But when providing language server support, we
  * need as much of the valid information to end up in a valid Registry as possible, so that code completion, go to
  * definition, find symbols, and many other features can still work even when the code has some errors.
  *
  * Compilations should be used to represent computation that can result in a <b>user error</b>. Internal assertions
  * don't need to be wrapped in a Compilation and should instead lead to a [[CompilationException]], as they signal
  * compiler bugs instead.
  */
sealed trait Compilation[+A] {

  /**
    * All feedback carried forward by the compilation.
    */
  def feedback: Vector[Feedback]

  /**
    * Whether the compilation is successful.
    */
  def isSuccess: Boolean

  /**
    * Creates a new compilation with the given list of feedback attached.
    */
  def withFeedback(feedback2: Vector[Feedback]): Compilation[A] = this match {
    case Success(result, feedback) =>
      if (feedback2.exists(_.isError)) {
        ResultFailure(result, feedback ++ feedback2)
      } else {
        Success(result, feedback ++ feedback2)
      }

    case ResultFailure(result, feedback) => ResultFailure(result, feedback ++ feedback2)
    case EmptyFailure(feedback) => EmptyFailure(feedback ++ feedback2)
  }

  /**
    * Maps the result of the compilation to another compilation. In the context of compilations, flatMap is to be
    * understood as a "chain of computation" which is broken as soon as the first empty compilation appears.
    */
  def flatMap[B](f: A => Compilation[B]): Compilation[B] = this match {
    case result: Result[A] => f(result.result).withFeedback(result.feedback)
    case failure: Failure[B] => failure
  }

  /**
    * Maps the result of type A to a new result of type B.
    */
  def map[B](f: A => B): Compilation[B] = flatMap(a => Success(f(a), Vector.empty))

  /**
    * Invokes the function `f` with the result of the compilation.
    */
  def foreach(f: A => Unit): Unit = map(f)

  /**
    * Converts the compilation into a verification, effectively discarding the result.
    */
  def verification: Verification = map(_ => ())

  /**
    * Returns this compilation or, if it is empty, a [[ResultFailure]] with the given default result.
    */
  def withDefault[B >: A](default: => B): Result[B] = this match {
    case result: Result[A] => result
    case EmptyFailure(feedback) => ResultFailure(default, feedback)
  }

  /**
    * Maps the result of a successful compilation to a compilation resulting in B.
    */
  def flatMapSuccess[B](f: A => Compilation[B]): Compilation[B] = this match {
    case Success(result, feedback) => f(result).withFeedback(feedback)
    case failure: Failure[_] => EmptyFailure(failure.feedback)
  }

}

object Compilation {

  /**
    * Signifies that the compilation produced a result of type A.
    *
    * Use this trait in type declarations if you are sure that the compilation must produce a result. This signals that
    * there will always be some kind of result, which can be beneficial for understanding compiler correctness.
    */
  sealed trait Result[+A] extends Compilation[A] {
    def result: A

    override def withFeedback(feedback2: Vector[Feedback]): Result[A] = {
      super.withFeedback(feedback2).asInstanceOf[Result[A]]
    }

    def flatMap[B](f: A => Result[B]): Result[B] = f(result).withFeedback(feedback)

    override def map[B](f: A => B): Result[B] = super.map(f).asInstanceOf[Result[B]]
  }

  /**
    * The compilation produced a result of type A and is a success.
    */
  case class Success[+A](result: A, feedback: Vector[Feedback]) extends Result[A] {
    if (feedback.exists(_.isError)) {
      throw CompilationException("Compilation.Successes may not contain errors.")
    }

    override def isSuccess = true
  }

  /**
    * The compilation is a failure.
    */
  sealed trait Failure[+A] extends Compilation[A] {
    override def isSuccess = false
  }

  /**
    * The compilation produced a result of type A, but is also a failure due to at least one error.
    */
  case class ResultFailure[+A](result: A, feedback: Vector[Feedback]) extends Failure[A] with Result[A]

  /**
    * The compilation produced no result and is a failure due to at least one error.
    */
  case class EmptyFailure(feedback: Vector[Feedback]) extends Failure[Nothing]

  def succeed[A](result: A): Success[A] = Success(result, Vector.empty)
  def fail[A](result: A, errors: Vector[Feedback.Error]): ResultFailure[A] = ResultFailure(result, errors)
  def fail[A](result: A, error: Feedback.Error): ResultFailure[A] = fail(result, Vector(error))
  def fail(errors: Vector[Feedback.Error]): EmptyFailure = EmptyFailure(errors)
  def fail(error: Feedback.Error): EmptyFailure = fail(Vector(error))

  /**
    * An abbreviation for Compilation[Unit]. A verification is a compilation that returns nothing of note when it is
    * successful.
    */
  type Verification = Compilation[Unit]

  object Verification {
    def succeed: Verification = Compilation.succeed(())

    // TODO: fromErrors should be represented in terms of `Verification.succeed` and `simultaneous`.
  }

  implicit class CompilationVectorExtension[A](compilations: Vector[Compilation[A]]) {
    /**
      * Combines all compilations into a single compilation. All results are added to a result vector. Missing results
      * from empty failures are skipped. All feedback is carried over to the combined compilation.
      *
      * This function does not produce an [[EmptyFailure]]. An empty result list is treated as a [[ResultFailure]].
      */
    def simultaneous: Result[Vector[A]] = simultaneous(ignoreResultFailures = false)

    /**
      * Combines all compilations into a single compilation. All successful results are added to a result vector.
      * Empty failures and result failures are skipped. All feedback is carried over to the combined compilation.
      *
      * This function does not produce an [[EmptyFailure]]. An empty result list is treated as a [[ResultFailure]].
      */
    def simultaneousSuccesses: Result[Vector[A]] = simultaneous(ignoreResultFailures = true)

    /**
      * @param ignoreResultFailures Ignore results carried by [[ResultFailure]] elements.
      */
    private def simultaneous(ignoreResultFailures: Boolean): Result[Vector[A]] = {
      var results: Vector[A] = Vector.empty
      var feedback: Vector[Feedback] = Vector.empty

      // There is a special case where we don't have any errors but the compilation still failed. Hence, we can't rely
      // on whether the error list is empty or not and have to manually remember whether there is any failure.
      var hasFailed = false

      compilations.foreach { compilation =>
        feedback = feedback ++ compilation.feedback

        compilation match {
          case Success(value, _) => results = results :+ value

          case failure: Failure[_] =>
            hasFailed = true
            failure match {
              case ResultFailure(value, _) if !ignoreResultFailures => results = results :+ value
              case _ =>
            }
        }
      }

      if (hasFailed) ResultFailure(results, feedback)
      else Success(results, feedback)
    }
  }

  implicit class CompilationTuple2Extension[A, B](tuple: (Compilation[A], Compilation[B])) {
    /**
      * Combines all compilations into a single compilation that yields the individual results in a tuple. Any empty
      * failure results in an empty failure overall. All feedback is carried over to the combined compilation.
      */
    def simultaneous: Compilation[(A, B)] = tuple match {
      case (Success(result1, feedback1), Success(result2, feedback2)) => Success((result1, result2), feedback1 ++ feedback2)
      case (c1: Result[A], c2: Result[B]) => ResultFailure((c1.result, c2.result), c1.feedback ++ c2.feedback)
      case (c1, c2) => EmptyFailure(c1.feedback ++ c2.feedback)
    }
  }

  implicit class ResultTuple2Extension[A, B](tuple: (Result[A], Result[B])) {
    def simultaneous: Result[(A, B)] = CompilationTuple2Extension(tuple).simultaneous.asInstanceOf[Result[(A, B)]]
  }

  implicit class CompilationTuple3Extension[A, B, C](tuple: (Compilation[A], Compilation[B], Compilation[C])) {
    /**
      * Combines all compilations into a single compilation that yields the individual results in a tuple. Any empty
      * failure results in an empty failure overall. All feedback is carried over to the combined compilation.
      */
    def simultaneous: Compilation[(A, B, C)] = tuple match {
      case (Success(result1, feedback1), Success(result2, feedback2), Success(result3, feedback3)) =>
        Success((result1, result2, result3), feedback1 ++ feedback2 ++ feedback3)
      case (c1: Result[A], c2: Result[B], c3: Result[C]) => ResultFailure((c1.result, c2.result, c3.result), c1.feedback ++ c2.feedback ++ c3.feedback)
      case (c1, c2, c3) => EmptyFailure(c1.feedback ++ c2.feedback ++ c3.feedback)
    }
  }

  implicit class ResultTuple3Extension[A, B, C](tuple: (Result[A], Result[B], Result[C])) {
    def simultaneous: Result[(A, B, C)] = CompilationTuple3Extension(tuple).simultaneous.asInstanceOf[Result[(A, B, C)]]
  }

  /**
    * A type `Option[Compilation[A]]` models an optional compilation resulting in a value of type A. For example,
    * we might only want to compile something if some value of Option[T] exists, then map it to a compilation:
    * `option.map(compile): Option[Compilation[A]]`.
    */
  implicit class OptionalCompilationExtension[A](option: Option[Compilation[A]]) {
    /**
      * Turns the optional compilation "inside out", which means that:
      *   - If the present value is None, we didn't actually compile anything, and so we claim that we have succeeded
      *     in compiling to a None value: Compilation.succeed(None).
      *   - If the present value is Some, we compiled something, and so we map the value of the compilation to Some.
      *
      * This is useful if we have a compilation that is optional, but then want to treat the case where no compilation
      * happened as if the compilation resulted in a None value.
      */
    def toCompiledOption: Compilation[Option[A]] = option match {
      case None => Compilation.succeed(None)
      case Some(compilation) => compilation.map(Some(_))
    }
  }

  implicit class OptionalResultExtension[A](option: Option[Result[A]]) {
    def toCompiledOption: Result[Option[A]] = OptionalCompilationExtension(option).toCompiledOption.asInstanceOf[Result[Option[A]]]
  }

  implicit class ToCompilationExtension[A](value: A) {
    /**
      * Creates a succeeding compilation with the given value.
      */
    def compiled: Success[A] = Compilation.succeed(value)
  }

  implicit class FoldCompilationsExtension[A](vector: Vector[A]) {
    /**
      * Lifts the vector's fold operation into a compilation context. The fold uses [[Compilation.flatMap]], so it
      * continues until an empty compilation is encountered.
      */
    def foldCompiled[B](initial: B)(f: (B, A) => Compilation[B]): Compilation[B] = {
      vector.foldLeft(Compilation.succeed(initial): Compilation[B]) { case (compilation, element) => compilation.flatMap(f(_, element)) }
    }

    /**
      * Lifts the vector's fold operation into a compilation context. The fold always consumes the whole list. If an
      * empty failure is encountered, the fold continues with the last valid value of B.
      */
    def foldSimultaneous[B](initial: B)(f: (B, A) => Compilation[B]): Result[B] = {
      vector.foldLeft(Compilation.succeed(initial): Result[B]) { case (result, element) =>
        f(result.result, element) match {
          case result2: Result[B] => result2.withFeedback(result.feedback)
          case _: EmptyFailure => result
        }
      }
    }
  }

  implicit class FilterCompilationExtension[A](vector: Vector[A]) {
    /**
      * Filters out duplicate elements in respect to the key produced by the `key` function. For any group of
      * duplicates, the `duplicate` function is consulted with a representative element of that group to produce an
      * error.
      */
    def filterDuplicates[K](key: A => K, duplicate: A => Feedback.Error): Result[Vector[A]] = {
      vector.groupBy(key).values.map {
        case Vector(a) => Compilation.succeed(a)
        case group if group.size > 1 => Compilation.fail(duplicate(group.head))
      }.toVector.simultaneous
    }
  }

}
