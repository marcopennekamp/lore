package lore.compiler.test

import lore.compiler.build.{BuildApi, BuildOptions}
import lore.compiler.feedback.{Feedback, MemoReporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.utils.CollectionExtensions.VectorExtension
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

import java.nio.file.Path

trait BaseSpec extends AnyFlatSpec with Matchers with OptionValues with Inside with Inspectors {

  private val testFragmentBase: Path = Path.of("compiler", "test", "lore", "compiler")

  /**
    * Analyzes the given fragment path with the default build options, producing a Registry.
    */
  def analyzeFragment(fragmentPath: String): Registry = {
    implicit val reporter: MemoReporter = MemoReporter()
    val registry = BuildApi.analyze(BuildOptions().withSources(testFragmentBase.resolve(fragmentPath)), exitEarly = true)

    val errors = reporter.feedback.filter(_.isError)
    if (errors.nonEmpty) {
      Assertions.fail(s"Compilation of $fragmentPath should have succeeded, but unexpectedly failed with errors:\n${errors.mkString("\n")}")
    }

    registry
  }

  /**
    * Assert that the given named source's compilation results in a list of errors, as required by the assertion.
    * The list of errors is passed as sorted into the assertion function, in order of lines starting from line 1.
    */
  def assertCompilationErrors(fragmentPath: String)(assert: Vector[Feedback.Error] => Assertion): Assertion = {
    implicit val reporter: MemoReporter = MemoReporter()
    BuildApi.analyze(BuildOptions().withSources(testFragmentBase.resolve(fragmentPath)), exitEarly = true)

    if (!reporter.feedback.exists(_.isError)) {
      Assertions.fail(s"Compilation of $fragmentPath should have failed with errors, but unexpectedly succeeded.")
    }

    val errors = Feedback.sort(reporter.feedback).filterType[Feedback.Error]
    assert(errors)
  }

  /**
    * A signature class that allows matching errors declaratively. You can extend this class to implement custom
    * match functionality.
    */
  case class ErrorSignature(errorClass: Class[_], expectedLine: Int) {
    def assertMatches(error: Feedback.Error): Assertion = {
      error.getClass shouldEqual errorClass
      error.position.line shouldEqual expectedLine
    }
  }

  /**
    * Matches error lists that match the given list of error signatures. Errors have to match in order.
    */
  def assertErrorsMatchSignatures(errors: Vector[Feedback.Error], signatures: Vector[ErrorSignature]): Assertion = {
    errors should have length signatures.length
    forAll(errors.zip(signatures)) { case (error, signature) =>
      signature.assertMatches(error)
    }
  }

  val beAbstract: Matcher[FunctionDefinition] = (f: FunctionDefinition) => MatchResult(f.isAbstract, s"$f was not abstract", s"$f was abstract")

  /**
    * Checks that the assertion is within the milliseconds time limit.
    */
  def timed(timeLimit: Int)(assertion: () => Assertion): Assertion = {
    val time1 = System.currentTimeMillis
    assertion()
    val time2 = System.currentTimeMillis
    val difference = (time2 - time1).toInt
    difference should be < timeLimit
  }

}
