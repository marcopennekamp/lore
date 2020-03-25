package lore.test

import lore.execution.Context
import lore.execution.Context.VerificationSuccess
import lore.functions.{LoreFunction, MultiFunction}
import lore.types.Type
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

abstract class BaseSpec extends AnyFlatSpec with Matchers with OptionValues with Inside with Inspectors {

  lazy val abstractContext: Context = Context.fromExample("abstract").value
  lazy val areaContext: Context = Context.fromExample("area").value
  lazy val concatContext: Context = Context.fromExample("concat").value

  def prepareContext(exampleName: String): Context = {
    val context = Context.fromExample(exampleName).value
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
