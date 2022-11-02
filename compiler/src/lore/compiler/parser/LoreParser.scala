package lore.compiler.parser

import fastparse.ParserInput
import lore.compiler.core.{Fragment, Position}
import lore.compiler.syntax.{DeclNode, ExprNode, TypeExprNode}
import lore.compiler.syntax.DeclNode.{GlobalVariableNode, ImportNode, ModuleNode}
import lore.compiler.syntax.Node.{NameNode, NamePathNode}

import scala.collection.mutable

object LoreParser {
  /**
    * TODO (syntax): Document.
    */
  def parse(input: String)(implicit fragment: Fragment): Option[ModuleNode] = new LoreParser(input).parse()

  def main(args: Array[String]): Unit = {
    val source =
      """
        |let abc = TODO
        |
        |module Hello
        |
        |  module World
        |     let me = TODO
        |  module Meme
        |module Dead
        |
        |
        |    let abcd = TODO
        |    module Holy
        |
        |""".stripMargin
    parse(source)(Fragment("test_fragment", ParserInput.fromString(source)))
  }
}

// TODO (syntax): Implement syntax error reporting with `reporter` and good error messages...
//                However, we will need to collect errors in a result structure first (replacing `Option`) to support
//                backtracking, which will require a minor or major parser rewrite because errors will have to be
//                concatenated.

/**
  * Implementation conventions:
  *   - EVERY production must either apply in full or not at all. `offset` may only be changed after the end of a
  *     production if it applied. Such a production is called <i>offset-conservative</i>.
  *   - Backtracking is implemented explicitly by recovering from productions that returned `false` or `None`.
  */
private class LoreParser(val input: String)(implicit fragment: Fragment) {
  private val EOF = '\u0000'

  private var offset: Int = 0
//  private var indentationLevels: List[Int] = List(0)

  def parse(): Option[ModuleNode] = {
    fragment()
    // TODO (syntax): Only return `Some` if `offset` is at EOF.
  }

  def fragment(): Option[ModuleNode] = {
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Modules.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO (syntax): Parse `@root`.
  def module(indentation: Int): Option[ModuleNode] = saveOffset {
    val startIndex = offset
    if (word("module") && ws()) {
      namePath().map { moduleName =>
        val (imports, members) = indent(indentation)
          .flatMap(bodyIndentation => moduleBody(bodyIndentation))
          .getOrElse((Vector.empty, Vector.empty))
        ModuleNode(moduleName, atRoot = false, imports, members, createPositionFrom(startIndex))
      }
    } else None
  }

  // TODO (syntax): Parse imports.
  def moduleBody(indentation: Int): Option[(Vector[ImportNode], Vector[DeclNode])] = {
    println(s"Module body indentation: $indentation")
    // TODO (syntax): We can find out which production to invoke right away by looking at the module member's first
    //                keyword. No backtracking with `orElse` required.
    def member = module(indentation) orElse globalVariable(indentation)
    val members = collectSep(member, nextLineExactIndentation(indentation))
    Some(Vector.empty, members)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Global variables.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def globalVariable(indentation: Int): Option[GlobalVariableNode] = saveOffset {
    val startIndex = offset
    if (word("let") && ws()) {
      name().flatMap { variableName =>
        ws()
        // TODO (syntax): Global variable type...
        ws()
        if (oneChar('=')) {
          // TODO (syntax): Parse expressions...
          ws()
          word("TODO")
          Some(
            GlobalVariableNode(
              variableName,
              TypeExprNode.UnitNode(Position.unknown),
              ExprNode.TupleNode(Vector.empty, Position.unknown),
              createPositionFrom(startIndex),
            )
          )
        } else None
      }
    } else None
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Functions and domains.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Names and identifiers.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def name(): Option[NameNode] = withPosition(identifier()).map(NameNode.tupled)

  def namePath(): Option[NamePathNode] = genericNamePath(name())

  private def genericNamePath(name: => Option[NameNode]): Option[NamePathNode] = {
    val names = collectSep(name, '.')
    if (names.isEmpty) return None
    Some(NamePathNode(names))
  }

  // TODO (syntax): Ensure that `identifier` isn't a keyword.
  def identifier(): Option[String] = {
    val firstChar = peek
    if (!isLetter(firstChar) && !isDigit(firstChar)) return None
    consume()

    val builder = new mutable.StringBuilder()
    builder.append(firstChar)

    charsWhile(
      c => isLetter(c) || isDigit(c) || c == '_' || isIdentifierSpecialCharacter(c),
      builder.append(_),
    )

    Some(builder.toString())
  }

  def int(): Option[Long] = {
    ???
  }

  def real(): Option[Double] = {
    ???
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Indentation.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Attempts to open a new indentation block on the next non-blank line. Returns the next line's indentation if it's
    * higher than `parentIndentation`. `indent` expects to be at the end of the line; if instead another token is
    * encountered, `indent` will return `None`.
    */
  def indent(parentIndentation: Int): Option[Int] = saveOffset {
    if (nextNonBlankLine()) {
      val startOffset = offset
      chars(' ')
      val indentation = offset - startOffset
      if (parentIndentation < indentation) Some(indentation) else None
    } else None
  }

  /**
    * Checks the current indentation at the START of the line against the expected indentation.
    */
  def exactIndentation(expectedIndentation: Int): Boolean = saveOffset {
    val startOffset = offset
    chars(' ')
    val indentation = offset - startOffset
    indentation == expectedIndentation
  }

  def nextLineExactIndentation(expectedIndentation: Int): Boolean =
    saveOffset(nextNonBlankLine() && exactIndentation(expectedIndentation))

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Whitespace and comments.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Consumes whitespace except for newlines, returning `true` if at least one character was consumed.
    */
  def ws(): Boolean = repeat(spacesOrTabs() || comment())

  def wl(): Boolean = repeat(ws() || newline())

  def spacesOrTabs(): Boolean = charsWhile(c => c == ' ' || c == '\t')

  def newline(): Boolean = oneChar('\n') || word("\r\n")

  def lineComment(): Boolean = {
    if (word("--")) {
      while (peek != '\n' && (peek != '\r' || peek(2) != '\n')) {
        consume()
      }
      true
    } else false
  }

  // TODO (syntax): Support block comments.
  def comment(): Boolean = lineComment()

  def blankLines(): Boolean = repeat(saveOffset(ws() *> newline()))

  /**
    * Moves the cursor to the next non-blank line.
    */
  def nextNonBlankLine(): Boolean = {
    if (saveOffset(ws() *> newline())) {
      // Ignore subsequent blank lines.
      blankLines()
      true
    } else false
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Input helpers.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def peek: Char = peek(1)

  def peek(n: Int): Char = {
    val i = offset + n - 1
    if (i < input.length) input.charAt(i) else EOF
  }

  def consume(): Char = {
    val c = peek
    if (c != EOF) offset += 1
    c
  }

  /**
    * Consumes characters while `predicate` holds and returns `true` if at least one character was consumed.
    */
  def charsWhile(predicate: Char => Boolean): Boolean = {
    var consumed = false
    while (predicate(peek)) {
      consume()
      consumed = true
    }
    consumed
  }

  /**
    * Consumes characters while `predicate` holds and returns `true` if at least one character was consumed.
    */
  def charsWhile(predicate: Char => Boolean, apply: Char => Unit): Boolean = {
    var consumed = false
    while (predicate(peek)) {
      apply(consume())
      consumed = true
    }
    consumed
  }

  /**
    * Consumes `c` if it matches the next character.
    */
  def oneChar(c: Char): Boolean = {
    if (peek == c) {
      consume()
      true
    } else false
  }

  /**
    * Consumes `c` as long as it matches the next character. Returns `true` if at least one character was consumed.
    */
  def chars(c: Char): Boolean = {
    var consumed = false
    while (peek == c) {
      consume()
      consumed = true
    }
    consumed
  }

  /**
    * Checks that the next characters match `string`, and advances `offset` by `string.length` and returns `true` if
    * `string` was matched.
    */
  def word(string: String): Boolean = {
    if (input.startsWith(string, offset)) {
      offset += string.length
      true
    } else false
  }

  /**
    * Repeats `action` until it returns `false`. Returns `true` if at least one action was successfully executed.
    *
    * `action` must be offset-conservative.
    */
  def repeat(action: => Boolean): Boolean = {
    var consumed = false
    while (action) {
      consumed = true
    }
    consumed
  }

  // TODO (syntax): Share implementation with `repeat`.
  def repeatSep(action: => Boolean, separator: Char): Boolean = {
    var consumed = false
    while (action) {
      consumed = true
      if (peek == separator) consume()
      else return true
    }
    consumed
  }

  /**
    * Collects results from `get` until it returns `None`.
    *
    * `get` must be offset-conservative.
    */
  def collect[A](get: => Option[A]): Vector[A] = {
    var results = Vector.empty[A]
    var ended = false
    while (!ended) {
      get match {
        case Some(result) => results :+= result
        case None => ended = true
      }
    }
    results
  }

  /**
    * Collects results from `get` until it returns `None`, requiring a `separator` between each production.
    *
    * `get` must be offset-conservative.
    *
    * TODO (syntax): Share implementation with `collect`.
    */
  def collectSep[A](get: => Option[A], separator: => Boolean): Vector[A] = {
    var results = Vector.empty[A]
    var ended = false
    while (!ended) {
      get match {
        case Some(result) =>
          results :+= result
          if (!separator) ended = true
        case None => ended = true
      }
    }
    results
  }

  def collectSep[A](get: => Option[A], separator: Char): Vector[A] = collectSep(get, oneChar(separator))

  def withPosition[R](action: => Option[R]): Option[(R, Position)] = {
    val startOffset = offset
    val result = action
    val endOffset = offset
    result.map((_, Position(fragment, startOffset, endOffset)))
  }

  def createPositionFrom(startIndex: Int): Position = Position(fragment, startIndex, endIndex = offset)

  /**
    * Saves the current offset, resetting it after `action` if `action` is `false`.
    */
  def saveOffset(action: => Boolean): Boolean = {
    val savedOffset = offset
    val result = action
    if (!result) offset = savedOffset
    result
  }

  /**
    * Saves the current offset, resetting it after `action` if `action` is `None`.
    */
  def saveOffset[R](action: => Option[R]): Option[R] = {
    val savedOffset = offset
    val result = action
    if (result.isEmpty) offset = savedOffset
    result
  }

  implicit class AnyExtension(any: Any) {
    /**
      * Ignores the result on the left.
      */
    def *>(other: Boolean): Boolean = other
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Character queries.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def isLetter(c: Char): Boolean = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'
  def isDigit(c: Char): Boolean = '0' <= c && c <= '9'
  def isIdentifierSpecialCharacter(c: Char): Boolean = c == '!' || c == '?'
}
