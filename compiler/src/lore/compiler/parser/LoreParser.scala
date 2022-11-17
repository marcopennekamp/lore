package lore.compiler.parser

import fastparse.ParserInput
import lore.compiler.core.Fragment
import lore.compiler.syntax.DeclNode.ModuleNode
import lore.compiler.syntax.Node.NamePathNode

object LoreParser {
  /**
    * TODO (syntax): Document.
    */
  def parse(input: String)(implicit fragment: Fragment): Option[ModuleNode] = new LoreParser(input).parse()

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
        |
        |    let abcd: Alphabet = TODO
        |    module Holy
        |
        |""".stripMargin
    parse(source)(Fragment("test_fragment", ParserInput.fromString(source)))
  }
}

// TODO (syntax): Implement syntax error reporting with `reporter` and good error messages...
//                However, we will need to collect errors in a result structure first (replacing `Option`) to support
//                error backtracking, which will require a minor or major parser rewrite because errors will have to be
//                concatenated.

/**
  * Implementation conventions:
  *   - Failed parsers may be stuck at an advanced [[Parser.offset]] and must be backtracked explicitly with
  *     [[Parser.BooleanActionExtension.backtrack]] or [[Parser.OptionActionExtension.backtrack]]. Parsers that are
  *     marked as [[Parser.OffsetConservative]] don't need to be backtracked even if they fail. This convention
  *     improves performance by avoiding lots of saved offsets.
  */
private class LoreParser(override val input: String)(override implicit val fragment: Fragment)
  extends Parser with DeclarationParser with AnnotationParser with TypeParameterParser with TypeParser
    with PrecedenceParser with NameParser with IndentationParser with WhitespaceParser
{
  def parse(): Option[ModuleNode] = {
    parseFragment()
    // TODO (syntax): Only return `Some` if `offset` is at EOF.
  }

  def parseFragment(): Option[ModuleNode] = {
    // TODO (syntax): The top module declaration needs some special handling, as a `module X` declaration is only a top
    //                module if it has no body. So we have to get a `module()`, check if it has a body, and if it has,
    //                it's actually not a top module but the first module member!
    val startIndex = offset
    blankLines()
    println(s"Consumed whitespace and blank lines until offset $offset.")
    val result = moduleBody(0).map { case (imports, members) =>
      ModuleNode(NamePathNode.empty, atRoot = false, imports, members, createPositionFrom(startIndex))
    }
    println(s"End position: ${fragment.input.prettyIndex(offset)}")
    println(s"Peek char: `$peek`")
    println(s"Peek line: ${input.substring(offset).takeWhile(_ != '\n')}")

    wl()

    println(s"Parser result: $result")

    result.filter { _ =>
      if (peek != EOF) {
        println("ERROR: Parser did not read the whole file!")
        false
      } else true
    }
  }
}
