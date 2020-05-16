package lore.compiler

import lore.compiler.Compilation.Verification
import lore.compiler.feedback.{Error, Feedback, InfoFeedback}
import shapeless.ops.hlist.{RightFolder, Tupler}
import shapeless.syntax.std.tuple._
import shapeless.{Generic, HList, HNil, LUBConstraint, Poly2}

// TODO: Performance-wise, it might be worth to use doubly-linked lists here, if performance ever becomes a problem.

/**
  * Represents a compilation to a value of type A. Either results in a Result[A] or an Errors object.
  *
  * This trait should be used to represent compilations that can result in a <b>user error</b>. Internal assertions
  * don't need to be wrapped in a Compilation and should instead lead to runtime errors, as they uncover compiler
  * bugs instead of user errors.
  */
sealed trait Compilation[+A] {
  /**
    * A list of info feedback that is always carried forward through operations such as flatMap and combine.
    */
  def infos: List[InfoFeedback]

  /**
    * Returns the result value or the alternative if this compilation is an error.
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
    * Maps the result value of type A to a compilation resulting in B. In the context of compilations, flatMap is
    * to be understood as a "chain of computation" which is broken as soon as the first error appears.
    */
  def flatMap[B](f: A => Compilation[B]): Compilation[B] = this match {
    case Result(a, infos) => f(a).withInfos(infos)
    case _ => this.asInstanceOf[Compilation[B]]
  }

  /**
    * Maps the result value of type A to a new value of type B.
    */
  def map[B](f: A => B): Compilation[B] = flatMap(a => Result(f(a), List.empty))

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
  def require(p: A => Boolean)(errors: Error*): Compilation[A] = this match {
    case r@Result(a, infos) => if (p(a)) r else Errors(errors.toList, infos)
    case x => x
  }

  /**
    * Attaches the given list of infos to a copy of the current compilation.
    */
  def withInfos(infos: List[InfoFeedback]): Compilation[A] = this match {
    case Result(a, infos2) => Result(a, infos ::: infos2)
    case Errors(errors, infos2) => Errors(errors, infos ::: infos2)
  }

  /**
    * Applies f to all errors and infos contained in this compilation.
    */
  def applyToFeedback(f: Feedback => Unit): Compilation[A] = this match {
    case Result(_, infos) => infos.foreach(f); this
    case Errors(errors, infos) => errors.foreach(f); infos.foreach(f); this
  }

  /**
    * If this compilation has failed, try to recover from a given set of errors to a new compilation.
    */
  def recover[B >: A](f: PartialFunction[List[Error], Compilation[B]]): Compilation[B] = this match {
    case Result(_, _) => this
    case Errors(errors, infos) => if (f.isDefinedAt(errors)) f(errors).withInfos(infos) else this
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

case class Result[+A](value: A, override val infos: List[InfoFeedback]) extends Compilation[A] {
  override def isError = false
}
case class Errors[+A](errors: List[Error], override val infos: List[InfoFeedback]) extends Compilation[A] {
  override def isError = true
}

object Compilation {
  def fail(errors: Error*): Compilation[Nothing] = Errors(errors.toList, List.empty)
  def failInfo(errors: Error*)(infos: InfoFeedback*): Compilation[Nothing] = Errors(errors.toList, infos.toList)
  def succeed[A](a: A): Compilation[A] = Result(a, List.empty)
  def succeedInfo[A](a: A)(infos: InfoFeedback*): Compilation[A] = Result(a, infos.toList)

  /**
    * An abbreviation for Compilation[A].
    */
  type C[+A] = Compilation[A]

  /**
    * An abbreviation for Compilation[Unit]. A verification is an operation that returns nothing of note when
    * it is successful and fails with a set of errors if it isn't.
    */
  type Verification = Compilation[Unit]

  object Verification {
    def succeed: Verification = Compilation.succeed(())

    // TODO: Provide versions of map and flatMap that don't take an argument, so that we can write code like:
    //       verify(...).ifVerified { nextVerification }

    /**
      * Creates a verification result from the given error list. If the list is empty, the verification is assumed to
      * be successful. Otherwise, the verification fails with the given errors.
      */
    def fromErrors(errors: List[Error]): Verification = {
      if (errors.nonEmpty) Errors(errors, List.empty) else Result((), List.empty)
    }
  }

  implicit class CompilationListExtension[A](compilations: List[Compilation[A]]) {
    /**
      * Combines all the compilations from a list into a single compilation. If any of the compilations have
      * resulted in an error, the combined compilation results in an error. This operation collects all errors
      * in unspecified order.
      */
    def simultaneous: Compilation[List[A]] = {
      // We could also implement this using foldRight, which would perhaps be more functional in style, but the
      // present definition is actually easier to parse, as foldRight requires matching on two compilations for each
      // iteration.
      // This implementation, on the other hand, does not need to match on two compilations. We collect all values
      // and errors independently, then decide whether the combined compilation is a list of errors or results.
      // The decision is very simple: If the list of errors is not empty, there is at least one compilation that has
      // failed, and thus the whole compilation has failed with the given errors.
      var results = List.empty[A]
      var errors = List.empty[Error]
      var infos = List.empty[InfoFeedback]
      // We have to reverse the list first since prepending to results will effectively swap the direction of the list.
      compilations.reverse.foreach {
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

  object polyOp {
    import shapeless.::

    /**
      * A polymorphic function used for simultaneous reduction of HList compilations.
      */
    object simultaneous extends Poly2 {
      implicit def caseCompilation[A, B <: HList]: Case.Aux[Compilation[A], Compilation[B], Compilation[A :: B]] = at[Compilation[A], Compilation[B]] {
        case (Result(a, infosA), Result(b, infosB)) => Result(a :: b, infosA ::: infosB)
        case (ca, cb) =>
          // As either ca or cb is guaranteed to be a failed compilation, combine will simply aggregate the errors
          // and hence also produce an Errors object. This makes the type cast valid.
          List(ca, cb).simultaneous.asInstanceOf[Compilation[A :: B]]
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
}
