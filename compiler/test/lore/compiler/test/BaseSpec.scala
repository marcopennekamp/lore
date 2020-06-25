package lore.compiler.test

import lore.compiler.core.{Errors, Registry, Result}
import lore.compiler.feedback.Error
import lore.compiler.Lore
import lore.compiler.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.types.Type
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

trait BaseSpec extends AnyFlatSpec with Matchers with OptionValues with Inside with Inspectors {
  lazy val abstractRegistry: Registry = prepareRegistry("abstract")
  lazy val areaRegistry: Registry = prepareRegistry("area")
  lazy val concatRegistry: Registry = prepareRegistry("concat")

  def prepareRegistry(exampleName: String): Registry = Lore.fromExample(exampleName).toOption.map(_._1).value

  /**
    * Assert that the given named source's compilation results in a list of errors, as required by the assertion.
    * The list of errors is passed as sorted into the assertion function, in order of lines starting from line 1.
    */
  def assertCompilationErrors(name: String)(assert: List[Error] => Assertion): Assertion = {
    Lore.fromExample(name) match {
      case Result(_, _) => Assertions.fail(s"Compilation of $name should have failed with errors, but unexpectedly succeeded.")
      case Errors(errors, _) => assert(errors.sortWith { case (e1, e2) => e1.position < e2.position })
    }
  }

  /**
    * A signature class that allows matching errors declaratively. You can extend this class to implement custom
    * match functionality.
    */
  case class ErrorSignature(errorClass: Class[_], expectedLine: Int) {
    def assertMatches(error: Error): Assertion = {
      error.getClass shouldEqual errorClass
      error.position.line shouldEqual expectedLine
    }
  }

  /**
    * Matches error lists that match the given list of error signatures. Errors have to match in order.
    */
  def assertErrorsMatchSignatures(errors: List[Error], signatures: List[ErrorSignature]): Assertion = {
    errors should have length signatures.length
    forAll(errors.zip(signatures)) { case (error, signature) =>
      signature.assertMatches(error)
    }
  }

  implicit class MultiFunctionExtension(multiFunction: MultiFunctionDefinition) {
    def exactGet(inputType: Type): FunctionDefinition = multiFunction.exact(inputType).get
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
