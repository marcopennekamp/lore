package lore.test

import lore.execution.Context
import lore.execution.Context.VerificationSuccess
import lore.functions.{LoreFunction, MultiFunction}
import lore.types.Type
import org.scalatest._
import org.scalatest.matchers.{MatchResult, Matcher}

abstract class BaseSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {

  lazy val abstractContext = Context.fromExample("abstract")
  lazy val areaContext = Context.fromExample("area")
  lazy val concatContext = Context.fromExample("concat")

  def prepareContext(exampleName: String): Context = {
    val context = Context.fromExample(exampleName)
    context.verify() should be (VerificationSuccess)
    context
  }

  implicit class MultiFunctionExtension(multiFunction: MultiFunction) {
    def exact(inputType: Type): LoreFunction = multiFunction.function(inputType).get
  }

  val beAbstract = new Matcher[LoreFunction] {
    def apply(f: LoreFunction) = MatchResult(f.isAbstract, s"$f was not abstract", s"$f was abstract")
  }

}
