package lore.compiler.parser.lexer

import fastparse.ParserInput
import lore.compiler.core.Fragment
import lore.compiler.feedback.{MemoReporter, Reporter}
import lore.compiler.syntax._
import scalaz.Scalaz.ToOptionIdOps

import scala.collection.immutable.HashMap
import scala.collection.mutable

object LoreLexer {
  def tokenize(input: String)(implicit fragment: Fragment, reporter: Reporter): Option[IndexedSeq[Token]] =
    new LoreLexer(input).tokenize()

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
    tokenize(source)(Fragment("test_fragment", ParserInput.fromString(source)), MemoReporter())
      .foreach(_.foreach(println))
  }
}

private class LoreLexer(input: String)(implicit fragment: Fragment, reporter: Reporter) {
  private val EOF = '\u0000'

  private val tokens = IndexedSeq.newBuilder[Token]

  private var offset: Int = 0

  /**
    * All levels of indentation encountered by the lexer. The zero indentation is guaranteed to always be the bottom
    * element of the stack.
    */
  private val indentationLevels: mutable.Stack[Int] = mutable.Stack(0)

  private def currentIndentation: Int = indentationLevels.top

  def tokenize(): Option[IndexedSeq[Token]] = {
    //noinspection LoopVariableNotUpdated
    while (offset < input.length) {
      val startIndex = offset
      consume() match {
        case c if isIdentifierStart(c) => tokenizeIdentifier(c, startIndex)
        case '@' => if (!tokenizeAnnotation(startIndex)) return None // An error will already have been reported.

        // Values.
        case c if isDigit(c) => tokenizeNumber(c, startIndex)
        case '#' => if (!tokenizeSymbol(startIndex)) return None // An error will already have been reported.

        // Operators and special characters.
        case '(' => tokens += TkParenLeft(startIndex)
        case ')' => tokens += TkParenRight(startIndex)
        case '[' => tokens += TkBracketLeft(startIndex)
        case ']' => tokens += TkBracketRight(startIndex)
        case '{' => tokens += TkBraceLeft(startIndex)
        case '}' => tokens += TkBraceRight(startIndex)

        case '%' => consume() match {
          case '{' => tokens += TkShapeStart(startIndex)
          case _ => return None // TODO (syntax): Report error.
        }

        case ',' => tokens += TkComma
        case '.' => tokens += TkDot(startIndex)
        case ':' => tokens += TkColon
        case '_' => tokens += TkUnderscore(startIndex)

        case '=' => peek match {
          case '=' => consume(); tokens += TkEqualsEquals
          case '>' => consume(); tokens += TkArrow
          case _ => tokens += TkEquals
        }

        case '+' => tokenizeCompositeEquals(TkPlus(startIndex), TkPlusEquals)
        case '-' => peek match {
          case '-' => consume(); skipLineComment()
          case _ => tokenizeCompositeEquals(TkMinus, TkMinusEquals)
        }
        case '*' => tokenizeCompositeEquals(TkMul, TkMulEquals)
        case '/' => tokenizeCompositeEquals(TkDiv, TkDivEquals)

        case '<' => tokenizeCompositeEquals(TkLessThan, TkLessThanEquals)
        case '>' => tokenizeCompositeEquals(TkGreaterThan, TkGreaterThanEquals)

        case '&' => tokens += TkTypeAnd
        case '|' => tokens += TkTypeOr

        // Whitespace and comment handling. (Note that line comment skips are initiated in operator handling.)
        case '\n' => if (!handleNewline()) return None
        case '\r' => // just skip
        case ' ' | '\t' => skipSpacesAndTabs()

        case _ => return None // TODO (syntax): Report error.
      }
    }
    tokens.result().some
  }

  private val keywords: HashMap[String, Int => Token] = HashMap(
    "_" -> TkUnderscore,
    "and" -> TkAnd,
    "case" -> TkCase,
    "cond" -> TkCond,
    "do" -> TkDo,
    "domain" -> TkDomain,
    "else" -> TkElse,
    "extends" -> TkExtends,
    "false" -> TkFalse,
    "fixed" -> TkFixed,
    "for" -> TkFor,
    "func" -> TkFunc,
    "if" -> TkIf,
    "intrinsic" -> TkIntrinsic,
    "let" -> TkLet,
    "module" -> TkModule,
    "mut" -> TkMut,
    "not" -> TkNot,
    "object" -> TkObject,
    "or" -> TkOr,
    "proc" -> TkProc,
    "return" -> TkReturn,
    "spec" -> TkSpec,
    "struct" -> TkStruct,
    "then" -> TkThen,
    "trait" -> TkTrait,
    "true" -> TkTrue,
    "type" -> TkType,
    "use" -> TkUse,
    "var" -> TkVar,
    "where" -> TkWhere,
    "while" -> TkWhile,
    "yield" -> TkYield,
  )

  private def isKeyword(name: String): Boolean = keywords.contains(name)

  /**
    * Requirement: `firstChar` must be a letter or an underscore.
    */
  private def tokenizeIdentifier(firstChar: Character, startIndex: Int): Unit = {
    val id = lexIdentifier(firstChar)
    tokens += keywords.get(id)
      .map { construct => construct(startIndex) }
      .getOrElse(TkIdentifier(id, startIndex))
  }

  private def lexIdentifier(): Option[String] = {
    val firstChar = consume()
    if (isIdentifierStart(firstChar)) lexIdentifier(firstChar).some
    else None
  }

  private def lexIdentifier(firstChar: Character): String = {
    val builder = new mutable.StringBuilder()
    builder.append(firstChar)

    charsWhile(
      c => isLetter(c) || isDigit(c) || c == '_' || c == '!' || c == '?',
      builder.append(_),
    )

    builder.toString()
  }

  /**
    * Requirement: `firstChar` must be a digit.
    *
    * TODO (syntax): We can certainly make this more efficient by parsing the number right away instead of using
    *                `toLong`/`toDouble`.
    */
  private def tokenizeNumber(firstChar: Char, startIndex: Int) = {
    val builder = new mutable.StringBuilder()
    builder.append(firstChar)

    charsWhile(c => isDigit(c), builder.append(_))
    if (peek != '.') {
      tokens += TkInt(builder.toString().toLong, startIndex)
    } else {
      builder.append(".")
      charsWhile(c => isDigit(c), builder.append(_))
      tokens += TkReal(builder.toString().toDouble, startIndex)
    }
  }

  /**
    * Requirement: The `@` already has been consumed.
    */
  private def tokenizeAnnotation(startIndex: Int): Boolean = {
    val name = lexIdentifier().getOrElse(return false) // TODO (syntax): Report error.
    name match {
      case name if name == "where" || !isKeyword(name) =>
        tokens += TkAnnotation(name, startIndex)
        true

      case _ =>
        // TODO (syntax): Report error (cannot use a keyword as annotation).
        false
    }
  }

  /**
    * Requirement: The `#` already has been consumed.
    */
  private def tokenizeSymbol(startIndex: Int): Boolean = {
    val name = lexIdentifier().getOrElse(return false) // TODO (syntax): Report error.
    tokens += TkSymbol(name, startIndex)
    true
  }

  // TODO (syntax): Should be inline (Scala 3).
  private def tokenizeComposite(expected: Character, basic: => Token, composite: => Token): Unit = {
    if (peek == expected) {
      consume()
      tokens += composite
    } else {
      tokens += basic
    }
  }

  // TODO (syntax): Should be inline (Scala 3).
  private def tokenizeCompositeEquals(basic: => Token, composite: => Token): Unit =
    tokenizeComposite('=', basic, composite)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Whitespace and comment handling.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Adds a [[TkNewline]] token, and [[TkIndent]] or [[TkDedent]] based on the indentation of the next line. Returns
    * `false` if indentation is malformed.
    *
    * Requirement: `\n` has already been consumed.
    */
  private def handleNewline(): Boolean = {
    tokens += TkNewline

    // To find out the new indent, we have to measure the start and end index of the first non-blank line's indentation.
    var startIndex = offset
    skipSpacesAndTabs()

    // Skip blank lines.
    while (skipNewline()) {
      startIndex = offset
      skipSpacesAndTabs()
    }

    val endIndex = offset
    val newIndentation = endIndex - startIndex
    if (newIndentation > currentIndentation) {
      tokens += TkIndent
      indentationLevels.push(newIndentation)
    } else {
      while (newIndentation < currentIndentation) {
        tokens += TkDedent
        indentationLevels.pop()
      }

      if (newIndentation != currentIndentation) {
        // TODO (syntax): Report error.
        return false
      }
    }
    true
  }

  /**
    * Skips a newline if present and returns whether any newline was skipped.
    */
  private def skipNewline(): Boolean = peek match {
    case '\n' => consume(); true
    case '\r' => peek(2) match {
      case '\n' => offset += 2; true
      case _ => false
    }
    case _ => false
  }

  private def skipSpacesAndTabs(): Unit = charsWhile(c => c == ' ' || c == '\t')

  /**
    * Requirement: `--` has already been consumed.
    */
  private def skipLineComment(): Unit = while (!peekNewline) { consume() }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Character queries.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def isLetter(c: Char): Boolean = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'
  private def isDigit(c: Char): Boolean = '0' <= c && c <= '9'

  private def isIdentifierStart(c: Char): Boolean = isLetter(c) || c == '_'

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Input helpers.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def peek: Char = peek(1)

  private def peek(n: Int): Char = {
    val i = offset + n - 1
    if (i < input.length) input.charAt(i) else EOF
  }

  private def peekNewline: Boolean = peek == '\n' || peek == '\r' && peek(2) == '\n'

  private def consume(): Char = {
    val c = peek
    if (c != EOF) offset += 1
    c
  }

  /**
    * Consumes characters while `predicate` holds.
    *
    * TODO (syntax): Should be inline (Scala 3).
    */
  def charsWhile(predicate: Char => Boolean): Unit = {
    while (predicate(peek)) {
      consume()
    }
  }

  /**
    * Consumes characters while `predicate` holds and invokes `apply` with each character.
    *
    * TODO (syntax): Should be inline (Scala 3).
    */
  private def charsWhile(predicate: Char => Boolean, apply: Char => Unit): Unit = {
    while (predicate(peek)) {
      apply(consume())
    }
  }
}
