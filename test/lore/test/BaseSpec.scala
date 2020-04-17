package lore.test

import lore.Lore
import lore.compiler.Registry
import lore.definitions.{FunctionDefinition, MultiFunctionDefinition}
import lore.types.Type
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

abstract class BaseSpec extends AnyFlatSpec with Matchers with OptionValues with Inside with Inspectors {
  lazy val abstractRegistry: Registry = prepareRegistry("abstract")
  lazy val areaRegistry: Registry = prepareRegistry("area")
  lazy val concatRegistry: Registry = prepareRegistry("concat")

  def prepareRegistry(exampleName: String): Registry = {
    val registry = Lore.fromExample(exampleName).toOption.value
    // TODO: Verify the Registry, too. Probably using phase 3.
    //registry.verify() should be (VerificationSuccess)
    registry
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
