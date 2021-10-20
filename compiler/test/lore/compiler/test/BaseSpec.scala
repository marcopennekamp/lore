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
    val registry = BuildApi.analyzeExitEarly(BuildOptions().withSources(testFragmentBase.resolve(fragmentPath)))

    val errors = reporter.feedback.filter(_.isError)
    if (errors.nonEmpty) {
      Assertions.fail(s"Compilation of $fragmentPath should have succeeded, but unexpectedly failed with errors:\n${errors.mkString("\n")}")
    }

    registry.getOrElse(
      Assertions.fail(s"Compilation of $fragmentPath succeeded without errors, but didn't produce a Registry.")
    )
  }

  /**
    * Assert that the given fragment's compilation results in a list of errors, as required by the assertion. The list
    * of errors is passed as sorted into the assertion function, in order of lines starting from line 1.
    */
  def assertCompilationErrors(fragmentPath: String)(assert: Vector[Feedback.Error] => Assertion): Assertion = {
    implicit val reporter: MemoReporter = MemoReporter()
    BuildApi.analyzeExitEarly(BuildOptions().withSources(testFragmentBase.resolve(fragmentPath)))

    if (!reporter.feedback.exists(_.isError)) {
      Assertions.fail(s"Compilation of $fragmentPath should have failed with errors, but unexpectedly succeeded.")
    }

    val errors = Feedback.sort(reporter.feedback).filterType[Feedback.Error]
    assert(errors)
  }

  /**
    * Assert that the given fragment's compilation results in the given error messages at each specified start line.
    */
  def assertCompilationErrorMessages(fragmentPath: String)(messages: (String, Int)*): Assertion = {
    assertCompilationErrors(fragmentPath) {
      errors => errors.map(e => (e.message, e.position.startLine)) shouldEqual messages.toVector
    }
  }

  /**
    * A signature class that allows matching errors declaratively. You can extend this class to implement custom
    * match functionality.
    */
  case class ErrorSignature(errorClass: Class[_], expectedLine: Int) {
    def assertMatches(error: Feedback.Error): Assertion = {
      error.getClass shouldEqual errorClass
      error.position.startLine shouldEqual expectedLine
    }
  }

  /**
    * Assert that the given fragment's compilation results in the given error signatures.
    */
  def assertCompilationErrorSignatures(fragmentPath: String)(signatures: (Class[_], Int)*): Assertion = {
    assertCompilationErrors(fragmentPath) { errors =>
      errors should have length signatures.length
      forAll(errors.zip(signatures)) { case (error, (errorClass, line)) =>
        ErrorSignature(errorClass, line).assertMatches(error)
      }
    }
  }

  val beAbstract: Matcher[FunctionDefinition] = (f: FunctionDefinition) => MatchResult(f.isAbstract, s"$f was not abstract", s"$f was abstract")

}
