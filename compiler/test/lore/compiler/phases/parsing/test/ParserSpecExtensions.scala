package lore.compiler.phases.parsing.test

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.phases.parsing.Space
import lore.compiler.test.BaseSpec
import org.scalatest.Assertion

trait ParserSpecExtensions[Result] { base: BaseSpec =>
  def parser[_: P]: P[Result]

  implicit class CheckParseExtension(source: String) {
    private def parse[T](onSuccess: Result => T, onFailure: String => T): T = {
      def file[_: P] = P(Space.WL ~ parser ~ Space.WL ~ End)
      fastparse.parse(source, file(_)) match {
        case Parsed.Success(result, _) => onSuccess(result)
        case Parsed.Failure(_, _, extra) => onFailure(extra.trace().aggregateMsg)
      }
    }

    def -->(expected: Result): Assertion = {
      parse[Assertion](_ shouldEqual expected, message => fail(s"Couldn't parse $source. $message."))
    }

    def fails: Assertion = {
      parse[Assertion](result => fail(s"Parsing $source should have failed, but resulted in $result."), _ => succeed)
    }

    def parsed: Result = {
      parse[Result](identity, message => fail(s"Couldn't parse $source. $message."))
    }
  }
}
