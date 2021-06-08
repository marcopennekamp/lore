package lore.compiler.test

import lore.compiler.core.{Errors, Result}
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.types.ProductType
import lore.compiler.{CompilerOptions, Lore}
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

import java.nio.file.Path

trait BaseSpec extends AnyFlatSpec with Matchers with OptionValues with Inside with Inspectors {

  private val testFragmentBase: Path = Path.of("compiler", "test", "lore", "compiler")

  private implicit val options: CompilerOptions = CompilerOptions(runtimeLogging = false)

  def prepareRegistry(fragmentName: String): Registry = {
    Lore.fromSources(Path.of("."), testFragmentBase.resolve(fragmentName + ".lore")).toOption match {
      case Some((registry, _)) => registry
      case None => throw new RuntimeException(s"Compilation of test fragment $fragmentName failed!")
    }
  }

  /**
    * Assert that the given named source's compilation results in a list of errors, as required by the assertion.
    * The list of errors is passed as sorted into the assertion function, in order of lines starting from line 1.
    */
  def assertCompilationErrors(fragmentName: String)(assert: Vector[Feedback.Error] => Assertion): Assertion = {
    Lore.fromSources(Path.of("."), testFragmentBase.resolve(fragmentName + ".lore")) match {
      case Result(_, _) => Assertions.fail(s"Compilation of $fragmentName should have failed with errors, but unexpectedly succeeded.")
      case Errors(errors, _) => assert(errors.sortWith { case (e1, e2) => e1.position < e2.position })
    }
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

  implicit class MultiFunctionExtension(multiFunction: MultiFunctionDefinition) {
    def exactGet(inputType: ProductType): FunctionDefinition = multiFunction.exact(inputType).get
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
