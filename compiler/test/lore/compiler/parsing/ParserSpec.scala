package lore.compiler.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.core.Fragment
import lore.compiler.test.BaseSpec
import org.scalatest.Assertion

trait ParserSpec[Result] extends BaseSpec {
  def parser[_: P](implicit fragment: Fragment): P[Result]

  implicit class CheckParseExtension(source: String) {
    private implicit val fragment: Fragment = Fragment("test", source)

    private def parse[T](onSuccess: Result => T, onFailure: String => T): T = {
      def file[_: P] = P(Space.WL ~ parser ~ Space.WL ~ End)
      fastparse.parse(source, file(_)) match {
        case Parsed.Success(result, _) => onSuccess(result)
        case Parsed.Failure(_, _, extra) => onFailure(extra.trace().aggregateMsg)
      }
    }

    def succeeds: Assertion = {
      parse(_ => succeed, message => fail(s"Couldn't parse $source. $message."))
    }

    def fails: Assertion = {
      parse(result => fail(s"Parsing $source should have failed, but resulted in $result."), _ => succeed)
    }
  }
}
