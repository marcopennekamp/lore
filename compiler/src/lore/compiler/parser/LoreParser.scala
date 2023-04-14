package lore.compiler.parser

import fastparse.ParserInput
import lore.compiler.build.BuildApi
import lore.compiler.core.{CompilerOptions, Fragment}
import lore.compiler.feedback.{MemoReporter, Reporter}
import lore.compiler.parser.lexer.Lexer
import lore.compiler.syntax.DeclNode.ModuleNode
import lore.compiler.syntax.Node.NamePathNode
import lore.compiler.syntax.{TkEnd, Token}

object LoreParser {
  /**
    * TODO (syntax): Document.
    */
  def parse(tokens: IndexedSeq[Token])(implicit fragment: Fragment, reporter: Reporter): Option[ModuleNode] = {
    val parser = new LoreParser(tokens)
    parser.parse() match {
      case Success(result) => Some(result)
      case Failure =>
        reporter.report(parser.reporter.feedback)
        None
    }
  }

  def main(args: Array[String]): Unit = {
    val source =
      """let abc: String = TODO
        |
        |module Hello
        |
        |  module World
        |     let me: [Int] = TODO
        |
        |  module Meme
        |    let abc: String & Int | Int => Int = TODO
        |module Dead
        |
        |    type ABC = (String, Int, Real)
        |    struct S1 = Y
        |    object O1 = Z
        |    type StringOrInt =
        |      String |
        |        Int
        |
        |    let abcd: Alphabet = TODO
        |
        |    @root
        |    module Holy
        |
        |""".stripMargin

    implicit val fragment: Fragment = Fragment("test_fragment", ParserInput.fromString(source))
    implicit val reporter: MemoReporter = MemoReporter()
    implicit val compilerOptions: CompilerOptions = CompilerOptions()

    Lexer.tokenize(source).map(parse(_)) match {
      case Some(result) => println(s"Result: $result")
      case None => BuildApi.logCompilationFeedback(reporter, 0, 0)
    }
  }
}

/**
  * Implementation conventions:
  *   - Failed parsers may be stuck at an advanced [[Parser.offset]] and must be backtracked explicitly with
  *     [[Parser.BooleanActionExtension.backtrack]] or [[Parser.ResultActionExtension.backtrack]]. Parsers that are
  *     marked as [[Parser.StateConservative]] don't need to be backtracked even if they fail.
  *   - A parser that returns a failure should usually report an error, unless otherwise stated (see for example
  *     [[NameParser]]). Errors are backtracked together with the offset.
  */
private class LoreParser(override val tokens: IndexedSeq[Token])(override implicit val fragment: Fragment)
  extends Parser with DeclarationParser with AnnotationParser with TypeParser with TypeParameterParser
    with ExpressionParser with NameParser with PrecedenceParser
{
  def parse(): Result[ModuleNode] = {
    // TODO (syntax): The top module declaration needs some special handling, as a `module X` declaration is only a top
    //                module if it has no body. So we have to get a `module()`, check if it has a body, and if it has,
    //                it's actually not a top module but the first module member!
    val startToken = peek
    val result = moduleDeclarationBody().map { case (imports, members) =>
      val position = startToken.position.toEither(members.lastOption, imports.lastOption, startToken.position)
      ModuleNode(NamePathNode.empty, atRoot = false, imports, members, position)
    }
    println(s"End position: ${peek.position}")
    println(s"Peek token: `$peek`")
    println(s"Parser result: $result")

    if (!peekIs[TkEnd]) {
      // TODO (syntax): Report error: Parser did not read whole file.
      println("ERROR: Parser did not read the whole file!")
      return Failure
    }
    result
  }
}
