package lore.compiler.core

import lore.compiler.core.Compilation.{EmptyFailure, Failure, Result, ResultFailure, SevereFailure, Success, Verification, fail}
import lore.compiler.feedback.Feedback
import shapeless.ops.hlist.{RightFolder, Tupler}
import shapeless.{Generic, HList, HNil, LUBConstraint, Poly2}

/**
  * Represents a compilation to a result of type A. Compilations are defined in two dimensions: (1) whether a result
  * was produced, and (2) whether the compilation was successful, a failure, or a severe failure. Compilations without
  * a result are called empty. Empty compilations are always failures. Compilations with a result may or may not be a
  * failure. A severe failure is always an empty compilation.
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
    case SevereFailure(feedback) => SevereFailure(feedback ++ feedback2)
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
    * Maps the result of a successful compilation to a compilation resulting in B.
    */
  def flatMapSuccess[B](f: A => Compilation[B]): Compilation[B] = this match {
    case Success(result, feedback) => f(result).withFeedback(feedback)
    case failure: SevereFailure => failure
    case failure: Failure[_] => EmptyFailure(failure.feedback)
  }

  /**
    * Executes the function `f` with the result if the compilation is successful.
    *
    * TODO: Don't implement this, but rather let the clients pattern match on Success. This is only used once in the compiler.
    */
  def ifSuccess(f: A => Unit): Unit = this match {
    case Success(a, _) => f(a)
    case _ => ()
  }

  /**
    * Returns the successful result value, or throws the exception if this compilation is a failure.
    *
    * TODO: Don't implement this, but rather let the clients pattern match on Success. This is only used once in the compiler.
    */
  def getSuccessfulResult(exception: => Throwable): A = this match {
    case Success(value, _) => value
    case _ => throw exception
  }

}

object Compilation {

  /**
    * Signifies that the compilation produced a result of type A.
    */
  sealed trait Result[+A] { self: Compilation[A] =>
    def result: A
  }

  /**
    * The compilation produced a result of type A and is a success.
    */
  case class Success[+A](result: A, feedback: Vector[Feedback]) extends Compilation[A] with Result[A] {
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

  /**
    * The compilation produced no result and is a severe failure that will stop compilation at the next juncture.
    *
    * TODO: Do we need this?
    */
  case class SevereFailure(feedback: Vector[Feedback]) extends Failure[Nothing]

  def succeed[A](result: A): Compilation[A] = Success(result, Vector.empty)
  def fail[A](result: A, errors: Vector[Feedback.Error]): Compilation[A] = ResultFailure(result, errors)
  def fail[A](result: A, error: Feedback.Error): Compilation[A] = fail(result, Vector(error))
  def fail(errors: Vector[Feedback.Error]): Compilation[Nothing] = EmptyFailure(errors)
  def fail(error: Feedback.Error): Compilation[Nothing] = fail(Vector(error))
  def failSeverely(error: Feedback.Error): Compilation[Nothing] = SevereFailure(Vector(error))

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
      * from empty failures are skipped. All feedback is carried over to the combined compilation. If `compilations`
      * contains at least one severe failure, the resulting compilation is also a severe failure.
      *
      * This function does not produce an [[EmptyFailure]]. An empty result list is treated as a [[ResultFailure]].
      */
    def simultaneous: Compilation[Vector[A]] = {
      var results: Vector[A] = Vector.empty
      var feedback: Vector[Feedback] = Vector.empty

      // There is a special case where we don't have any errors but the compilation still failed. Hence, we can't rely
      // on whether the error list is empty or not and have to manually remember whether there is any failure.
      var hasFailed = false
      var isSevere = false

      compilations.foreach { compilation =>
        feedback = feedback ++ compilation.feedback

        compilation match {
          case Success(value, _) => results = results :+ value

          case failure: Failure[_] =>
            hasFailed = true
            failure match {
              case ResultFailure(value, _) => results = results :+ value
              case EmptyFailure(_) =>
              case SevereFailure(_) => isSevere = true
            }
        }
      }

      if (isSevere) SevereFailure(feedback)
      else if (hasFailed) ResultFailure(results, feedback)
      else Success(results, feedback)
    }
  }

  object polyOp {
    import shapeless.::

    /**
      * A polymorphic function used for simultaneous reduction of HList compilations.
      */
    object simultaneous extends Poly2 {
      implicit def caseCompilation[A, B <: HList]: Case.Aux[Compilation[A], Compilation[B], Compilation[A :: B]] = at[Compilation[A], Compilation[B]] {
        case (SevereFailure(feedback1), c2) => SevereFailure(feedback1 ++ c2.feedback)
        case (c1, SevereFailure(feedback2)) => SevereFailure(c1.feedback ++ feedback2)

        case (Success(result1, feedback1), Success(result2, feedback2)) => Success(result1 :: result2, feedback1 ++ feedback2)
        case (c1: Result[A], c2: Result[B]) => ResultFailure(c1.result :: c2.result, c1.feedback ++ c2.feedback)

        case (c1, c2) => EmptyFailure(c1.feedback ++ c2.feedback)
      }
    }
  }

  /**
    * Models simultaneous compilation on shapeless HLists.
    */
  implicit class CompilationHListExtension[T, L <: HList, RL <: HList, RT](compilations: T)(
    // Provides a means to convert the tuple T to the entry HList EL.
    implicit val gen: Generic.Aux[T, L],
    // Provides a means to fold the HList L to the result HList RL via the polyOp.simultaneous operator.
    val folder: RightFolder.Aux[L, Compilation[HNil], polyOp.simultaneous.type, Compilation[RL]],
    // Provides a means to convert the result HList to a result tuple.
    val tupler: Tupler.Aux[RL, RT],
    // Ensures that only Compilations are part of the input HList.
    val lub: LUBConstraint[L, Compilation[_]],
  ) {
    def simultaneous: Compilation[RT] = {
      // I can't believe this actually works. Many thanks to Travis Brown for his answers on StackOverflow.
      gen.to(compilations).foldRight(succeed(HNil: HNil))(polyOp.simultaneous).map(tupler(_))
    }
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

  implicit class ToCompilationExtension[A](value: A) {
    /**
      * Creates a succeeding compilation with the given value.
      */
    def compiled: Compilation[A] = Compilation.succeed(value)
  }

  implicit class FoldCompilationsExtension[A](vector: Vector[A]) {
    /**
      * Lifts the vector's fold operation into a compilation context. The fold uses [[Compilation.flatMap]], so it
      * continues until an empty compilation is encountered.
      */
    def foldCompiled[B](initial: B)(f: (B, A) => Compilation[B]): Compilation[B] = {
      vector.foldLeft(Compilation.succeed(initial)) { case (compilation, element) => compilation.flatMap(f(_, element)) }
    }

    /**
      * Lifts the vector's fold operation into a compilation context. The fold continues until a severe failure is
      * encountered. If an empty failure is encountered, the fold continues with the last valid value of B.
      */
    def foldSimultaneous[B](initial: B)(f: (B, A) => Compilation[B]): Compilation[B] = {
      vector.foldLeft(Compilation.succeed(initial)) { case (compilation, element) =>
        compilation match {
          case result: Result[B] =>
            f(result.result, element) match {
              case result2: Result[B] => result2.withFeedback(result.feedback)
              case _: EmptyFailure => result
              case failure2: SevereFailure => failure2.withFeedback(result.feedback)
            }

          case _: EmptyFailure => throw CompilationException("`foldCompiled` cannot result in a non-severe, empty failure.")
          case failure: SevereFailure => failure
        }
      }
    }
  }

}
