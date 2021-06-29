package lore.compiler.core

import lore.compiler.core.Compilation.Verification
import lore.compiler.feedback.Feedback
import shapeless.ops.hlist.{RightFolder, Tupler}
import shapeless.syntax.std.tuple._
import shapeless.{Generic, HList, HNil, LUBConstraint, Poly2}

/**
  * Represents a compilation to a value of type A. Either results in a Result[A] or an Errors object.
  *
  * This trait should be used to represent compilations that can result in a <b>user error</b>. Internal assertions
  * don't need to be wrapped in a Compilation and should instead lead to runtime errors, as they uncover compiler
  * bugs instead of user errors.
  */
sealed trait Compilation[+A] {
  /**
    * A list of warnings that is always carried forward through operations such as flatMap and combine.
    */
  def warnings: Vector[Feedback.Warning]

  /**
    * A combined list of all errors and warnings.
    */
  def feedback: Vector[Feedback]

  /**
    * Returns the result value, or the alternative if this compilation is an error.
    */
  def getOrElse[B >: A](alternative: => B): B = this match {
    case Result(a, _) => a
    case Errors(_, _) => alternative
  }

  /**
    * Whether the current compilation resulted in an error.
    */
  def isError: Boolean

  /**
    * Whether the current compilation is successful.
    */
  def isSuccess: Boolean = !isError

  /**
    * Maps the result value of type A to a compilation resulting in B. In the context of compilations, flatMap is
    * to be understood as a "chain of computation" which is broken as soon as the first error appears.
    */
  def flatMap[B](f: A => Compilation[B]): Compilation[B] = this match {
    case Result(a, warnings) => f(a).withWarnings(warnings)
    case _ => this.asInstanceOf[Compilation[B]]
  }

  /**
    * Maps the result value of type A to a new value of type B.
    */
  def map[B](f: A => B): Compilation[B] = flatMap(a => Result(f(a), Vector.empty))

  /**
    * Executes the function `f` with the contained value if the compilation is a result.
    */
  def foreach(f: A => Unit): Unit = this match {
    case Result(a, _) => f(a)
    case _ => ()
  }

  /**
    * Filters the result value, provided this compilation has been successful so far. If the predicate is false,
    * the compilation results in the given errors.
    */
  def require(p: A => Boolean)(errors: Feedback.Error*): Compilation[A] = this match {
    case r@Result(a, warnings) => if (p(a)) r else Errors(errors.toVector, warnings)
    case x => x
  }

  /**
    * Creates a new compilation with the given list of warnings attached. Previously existing warnings are preserved.
    */
  def withWarnings(warnings1: Vector[Feedback.Warning]): Compilation[A] = this match {
    case Result(a, warnings2) => Result(a, warnings1 ++ warnings2)
    case Errors(errors, warnings2) => Errors(errors, warnings1 ++ warnings2)
  }

  /**
    * Applies f to all errors and warnings contained in this compilation.
    */
  def applyToFeedback(f: Feedback => Unit): Compilation[A] = this match {
    case Result(_, warnings) => warnings.foreach(f); this
    case Errors(errors, warnings) => errors.foreach(f); warnings.foreach(f); this
  }

  /**
    * If this compilation has failed, try to recover from a given set of errors to a new compilation.
    */
  def recover[B >: A](f: PartialFunction[Vector[Feedback.Error], Compilation[B]]): Compilation[B] = this match {
    case Result(_, _) => this
    case Errors(errors, warnings) => if (f.isDefinedAt(errors)) f(errors).withWarnings(warnings) else this
  }

  /**
    * Converts the compilation to an option, discarding all feedback.
    */
  def toOption: Option[A] = this match {
    case Result(a, _) => Some(a)
    case Errors(_, _) => None
  }

  /**
    * Converts the compilation into a verification, effectively discarding the result value.
    */
  def verification: Verification = map(_ => ())
}

case class Result[+A](value: A, override val warnings: Vector[Feedback.Warning]) extends Compilation[A] {
  override def feedback: Vector[Feedback] = warnings
  override def isError = false
}

case class Errors[+A](errors: Vector[Feedback.Error], override val warnings: Vector[Feedback.Warning]) extends Compilation[A] {
  override def feedback: Vector[Feedback] = errors ++ warnings
  override def isError = true
}

object Compilation {
  def fail(errors: Feedback.Error*): Compilation[Nothing] = Errors(errors.toVector, Vector.empty)
  def succeed[A](a: A): Compilation[A] = Result(a, Vector.empty)

  /**
    * An abbreviation for Compilation[Unit]. A verification is an operation that returns nothing of note when
    * it is successful and fails with a set of errors if it isn't.
    */
  type Verification = Compilation[Unit]

  object Verification {
    def succeed: Verification = Compilation.succeed(())

    /**
      * Creates a verification result from the given error list. If the list is empty, the verification is assumed to
      * be successful. Otherwise, the verification fails with the given errors.
      */
    def fromErrors(errors: Vector[Feedback.Error]): Verification = {
      if (errors.nonEmpty) Errors(errors, Vector.empty) else succeed
    }
  }

  implicit class CompilationVectorExtension[A](compilations: Vector[Compilation[A]]) {
    /**
      * Combines all the compilations from an iterable into a single compilation. If any of the compilations have
      * resulted in an error, the combined compilation results in an error. This operation collects all errors
      * in unspecified order.
      */
    def simultaneous: Compilation[Vector[A]] = {
      var results: Vector[A] = Vector.empty
      var errors: Vector[Feedback.Error] = Vector.empty
      var warnings: Vector[Feedback.Warning] = Vector.empty
      var hasFailed = false
      compilations.foreach {
        case Result(value, warnings2) =>
          results = results :+ value
          warnings = warnings ++ warnings2
        case Errors(errors2, warnings2) =>
          hasFailed = true
          errors = errors ++ errors2
          warnings = warnings ++ warnings2
      }
      // There is a special case where we don't have any errors but the compilation still failed. Hence, we can't rely
      // on whether the error list is empty or not and have to check whether there is any failure.
      if (hasFailed) Errors(errors, warnings) else Result(results, warnings)
    }
  }

  object polyOp {
    import shapeless.::

    /**
      * A polymorphic function used for simultaneous reduction of HList compilations.
      */
    object simultaneous extends Poly2 {
      implicit def caseCompilation[A, B <: HList]: Case.Aux[Compilation[A], Compilation[B], Compilation[A :: B]] = at[Compilation[A], Compilation[B]] {
        case (Result(a, warningsA), Result(b, warningsB)) => Result(a :: b, warningsA ++ warningsB)
        case (ca, cb) =>
          // As either ca or cb is guaranteed to be a failed compilation, simultaneous will simply aggregate the errors
          // and hence also produce an Errors object. This makes the type cast valid.
          Vector(ca, cb).simultaneous.asInstanceOf[Compilation[A :: B]]
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
    * `maybeT.map(t => compile(t)): Option[Compilation[A]]`.
    */
  implicit class OptionalCompilationExtension[A](option: Option[Compilation[A]]) {
    /**
      * Turns the optional compilation "inside out", which means that:
      *   - If the present value is None, we didn't actually compile anything, and so we claim that we have
      *     succeeded in compiling to a None value: Compilation.succeed(None).
      *   - If the present value is Some, we compiled something, and so we map the value of the compilation
      *     to Some.
      *
      * This is useful if we have a compilation that is optional, but then want to treat the case where no
      * compilation happened as if the compilation resulted in a None value.
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
      * Lifts the vector's fold operation into a compilation context. Note that the operation is aborted when a single
      * error has been found.
      */
    def foldCompiled[B](initial: B)(f: (B, A) => Compilation[B]): Compilation[B] = {
      vector.foldLeft(Compilation.succeed(initial)) { case (compilation, element) => compilation.flatMap(f(_, element)) }
    }

    /**
      * Lifts the vector's fold operation into a compilation context. In contrast to `foldCompiled`, this function
      * continues with the last valid value of B if an error is encountered. Hence, `f` will be attempted for each
      * element of the vector.
      */
    def foldSimultaneous[B](initial: B)(f: (B, A) => Compilation[B]): Compilation[B] = {
      var b = initial
      var successWarnings = Vector.empty[Feedback.Warning]
      var failures = Vector.empty[Errors[B]]

      for (a <- vector) {
        f(b, a) match {
          case Result(value, warnings) =>
            b = value
            successWarnings = successWarnings ++ warnings

          case errors@Errors(_, _) =>
            failures = failures :+ errors
        }
      }

      if (failures.nonEmpty) {
        val errors = failures.flatMap(_.errors)
        val failureWarnings = failures.flatMap(_.warnings)
        Errors(errors, successWarnings ++ failureWarnings)
      } else {
        Result(b, successWarnings)
      }
    }
  }
}
