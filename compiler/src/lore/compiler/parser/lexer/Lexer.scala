package lore.compiler.parser.lexer

import fastparse.ParserInput
import lore.compiler.core.Fragment
import lore.compiler.feedback.{MemoReporter, Reporter}
import lore.compiler.syntax.Token.TokenIndex
import lore.compiler.syntax._
import scalaz.Scalaz.ToOptionIdOps

import scala.collection.immutable.HashMap
import scala.collection.mutable

object Lexer {
  def tokenize(input: String)(implicit fragment: Fragment, reporter: Reporter): Option[IndexedSeq[Token]] =
    new Lexer(input).tokenize()

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
        |    let abcd: Alphabet = 'A B C $d e f ${g + h} i j ${ k + %{ l: 5, m: 8 }.l } n o p'
        |    let nested: String = 'hello ${ %{ world: 'world ${ concat('!', '!') }' }.world }!'
        |
        |    @root
        |    module Holy
        |""".stripMargin
    tokenize(source)(Fragment("test_fragment", ParserInput.fromString(source)), MemoReporter())
      .foreach(_.foreach(println))
  }
}

class Lexer(input: String)(implicit fragment: Fragment, reporter: Reporter) {
  private val EOF = '\u0000'

  private var offset: Int = 0

  private val tokens = IndexedSeq.newBuilder[Token]

  /**
    * The current stack of modes the lexer is in.
    *
    * Because Lore currently doesn't support multi-line strings, if the lexer encounters a newline, its mode stack must
    * not include a `StringLexerMode`. Otherwise, a newline is contained in an interpolation, which is illegal.
    *
    * The last `CodeLexerMode` may never be popped off the stack. Otherwise, a brace imbalance has occurred. Also, at
    * the end of the file, if the mode stack isn't exactly `[CodeLexerMode]`, the lexer encountered too few closing
    * braces.
    */
  private val modes: mutable.Stack[LexerMode] = mutable.Stack(CodeLexerMode)

  /**
    * All levels of indentation encountered by the lexer. The zero indentation is guaranteed to always be the bottom
    * element of the stack.
    */
  private val indentationLevels: mutable.Stack[Int] = mutable.Stack(0)

  private def currentIndentation: Int = indentationLevels.top

  def tokenize(): Option[IndexedSeq[Token]] = {
    //noinspection LoopVariableNotUpdated
    while (offset < input.length) {
      val isValidRun = modes.top match {
        case CodeLexerMode => handleCodeMode()
        case StringLexerMode => handleStringMode()
      }
      if (!isValidRun) return None
    }

    // At the end of the file, there must be exactly one lexer mode remaining: `CodeLexerMode`.
    if (modes.length != 1 || modes.top != CodeLexerMode) {
      // TODO (syntax): Report error.
      println(s"Unbalanced modes: $modes")
      return None
    }

    // If the file doesn't end with a newline (ignoring trailing spaces and tabs), invoke the lexer as if it would.
    // This generates a newline token for the last line and dedents for unclosed indentation levels.
    if (input.findLast(c => !isSpaceOrTab(c)).exists(_ != '\n')) {
      handleNewline(offset)
    }

    tokens.result().some
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Code mode.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * For optimization, [[handleCodeMode]] consumes characters until the mode is switched. Returns `false` if the lexer
    * encountered an error.
    */
  private def handleCodeMode(): Boolean = {
    //noinspection LoopVariableNotUpdated
    while (offset < input.length) {
      val startIndex = offset
      consume() match {
        case c if isIdentifierStart(c) => tokenizeIdentifier(c, startIndex)
        case '@' => if (!tokenizeAnnotation(startIndex)) return false // An error will already have been reported.

        // Values.
        case c if isDigit(c) => tokenizeNumber(c, startIndex)
        case '#' => if (!tokenizeSymbol(startIndex)) return false // An error will already have been reported.

        // Strings.
        case '\'' =>
          pushMode(StringLexerMode)
          return true

        // Operators and special characters.
        case '(' => tokens += TkParenLeft(startIndex)
        case ')' => tokens += TkParenRight(startIndex)
        case '[' => tokens += TkBracketLeft(startIndex)
        case ']' => tokens += TkBracketRight(startIndex)

        case '{' =>
          pushMode(CodeLexerMode)
          tokens += TkBraceLeft(startIndex)

        case '}' =>
          val isBalanced = popMode(CodeLexerMode)
          if (!isBalanced) {
            // TODO (syntax): Report error.
            println(s"Cannot pop `CodeLexerMode` from: $modes")
            return false
          }

          // The closing brace is either the end of an interpolation or a simple right brace. This depends on whether
          // the new mode (after `CodeLexerMode` has been popped) is a `StringLexerMode`.
          tokens += (if (modes.top == StringLexerMode) TkInterpolationEnd(startIndex) else TkBraceRight(startIndex))
          return true

        case '%' => consume() match {
          case '{' =>
            pushMode(CodeLexerMode)
            tokens += TkShapeStart(startIndex)

          case c =>
            // TODO (syntax): Report error.
            println(s"Invalid token after `%`: `$c`")
            return false
        }

        case ',' => tokens += TkComma(startIndex)
        case '.' => tokens += TkDot(startIndex)
        case ':' => tokens += TkColon(startIndex)
        case '_' => tokens += TkUnderscore(startIndex)

        case '=' => peek match {
          case '=' => consume(); tokens += TkEqualsEquals(startIndex)
          case '>' => consume(); tokens += TkArrow(startIndex)
          case _ => tokens += TkEquals(startIndex)
        }

        case '+' => tokenizeCompositeEquals(TkPlus(startIndex), TkPlusEquals(startIndex))
        case '-' => peek match {
          case '-' => consume(); skipLineComment()
          case _ => tokenizeCompositeEquals(TkMinus(startIndex), TkMinusEquals(startIndex))
        }
        case '*' => tokenizeCompositeEquals(TkMul(startIndex), TkMulEquals(startIndex))
        case '/' => tokenizeCompositeEquals(TkDiv(startIndex), TkDivEquals(startIndex))

        case '<' => peek match {
          case ':' => consume(); tokens += TkTypeLessThan(startIndex)
          case _ => tokenizeCompositeEquals(TkLessThan(startIndex), TkLessThanEquals(startIndex))
        }
        case '>' => peek match {
          case ':' => consume(); tokens += TkTypeGreaterThan(startIndex)
          case _ => tokenizeCompositeEquals(TkGreaterThan(startIndex), TkGreaterThanEquals(startIndex))
        }

        case '&' => tokens += TkTypeAnd(startIndex)
        case '|' => tokens += TkTypeOr(startIndex)

        // Whitespace and comment handling. (Note that line comment skips are initiated in operator handling.)
        case '\n' =>
          val isValid = handleNewline(startIndex)
          if (!isValid) return false // An error will already have been reported.

        case '\r' => // just skip
        case ' ' | '\t' => skipSpacesAndTabs()

        case c =>
          // TODO (syntax): Report error.
          println(s"Unknown token in `code` mode: `$c` (next: `$peek`) (modes: $modes.top)")
          return false
      }
    }
    true
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // String mode.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * To properly build a string, [[handleStringMode]] consumes characters until the mode is switched. Returns `false`
    * if the lexer encountered an error.
    *
    * Note: The opening quote has already been consumed.
    */
  private def handleStringMode(): Boolean = {
    // The string's position either starts with the opening quote, or if that doesn't exist, with this first character
    // of the string.
    val stringStartIndex = if (peek(-1) == '\'') offset - 1 else offset
    val stringBuilder = new mutable.StringBuilder()

    def finishString(endIndex: TokenIndex): Unit = {
      if (stringBuilder.nonEmpty) {
        tokens += TkString(stringBuilder.toString(), stringStartIndex, endIndex)
      }
    }

    //noinspection LoopVariableNotUpdated
    while (offset < input.length) {
      consume() match {
        case '\'' =>
          finishString(offset)
          popMode(StringLexerMode)
          return true

        case '$' =>
          finishString(offset - 1)
          return handleInterpolation() // In case of an error, it will already have been reported.

        case '\n' => return false // TODO (syntax): Report error.

        case '\\' =>
          val isValid = appendEscapeSequence(stringBuilder)
          if (!isValid) return false // An error will already have been reported.

        case c => stringBuilder.append(c)
      }
    }
    true
  }

  /**
    * Requirement: `$` has already been consumed.
    */
  private def handleInterpolation(): Boolean = {
    val startIndex = offset
    tokens += TkInterpolationStart(startIndex, if (peek == '{') startIndex + 2 else startIndex + 1)

    consume() match {
      case '{' =>
        pushMode(CodeLexerMode)
        true

      case c if isIdentifierStart(c) =>
        tokenizeIdentifier(c, startIndex)
        tokens += TkInterpolationEnd(offset)
        true

      case _ => false // TODO (syntax): Report error.
    }
  }

  /**
    * Requirement: `\` has already been consumed.
    */
  private def appendEscapeSequence(stringBuilder: StringBuilder): Boolean = {
    val character = consume() match {
      case 'n' => '\n'
      case 'r' => '\r'
      case 't' => '\t'
      case '\'' => '\''
      case '$' => '$'
      case '\\' => '\\'

      case 'u' => consume(4) match {
        case Some(unicodeCode) =>
          try {
            Integer.parseInt(unicodeCode, 16).toChar
          } catch {
            case _: Throwable => return false // TODO (syntax): Report error.
          }

        case None => return false // TODO (syntax): Report error.
      }

      case _ => return false // TODO (syntax): Report error.
    }
    stringBuilder.append(character)
    true
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Identifiers.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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
    "open" -> TkOpen,
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Annotations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Values.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
      tokens += TkInt(builder.toString().toLong, startIndex, offset)
    } else {
      builder.append(".")
      charsWhile(c => isDigit(c), builder.append(_))
      tokens += TkReal(builder.toString().toDouble, startIndex, offset)
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Whitespace and comment handling.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Adds a [[TkNewline]] token, and [[TkIndent]] or [[TkDedent]] based on the indentation of the next line. Returns
    * `false` if indentation is malformed or the newline is encountered in string interpolation (see [[modes]]).
    *
    * Requirement: `\n` has already been consumed.
    */
  private def handleNewline(newlineIndex: TokenIndex): Boolean = {
    // Because Lore doesn't support multi-line strings, a newline may not be contained in an interpolation. The lexer
    // is currently parsing an interpolation if it has a `StringLexerMode`.
    if (modes.length > 1 && modes.contains(StringLexerMode)) return false // TODO (syntax): Report error.

    tokens += TkNewline(newlineIndex)

    // To find out the new indent, we have to measure the start and end index of the first non-blank line's indentation.
    var startIndex = offset
    skipSpacesAndTabs()

    // Skip blank lines until `skipSpacesAndTabs` has arrived at a non-newline character.
    while (skipNewline()) {
      startIndex = offset
      skipSpacesAndTabs()
    }

    val endIndex = offset
    val newIndentation = endIndex - startIndex
    if (newIndentation > currentIndentation) {
      tokens += TkIndent(startIndex)
      indentationLevels.push(newIndentation)
    } else {
      while (newIndentation < currentIndentation) {
        tokens += TkDedent(startIndex)
        indentationLevels.pop()
      }

      // TODO (syntax): "Lax" indentation, previously handled by `wlmi`, might cause issues here. Reconsider if this
      //                restriction is really necessary in all cases. For example, if a line ends with certain tokens
      //                like `+`, we might not want to handle indentation at all (except for ensuring a minimum
      //                indentation) and instead skip newlines. I think Scala has a similar approach.
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

  private def skipSpacesAndTabs(): Unit = charsWhile(isSpaceOrTab)

  /**
    * Requirement: `--` has already been consumed.
    */
  private def skipLineComment(): Unit = while (!peekNewline) { consume() }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Character queries.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def isSpaceOrTab(c: Char): Boolean = c == ' ' || c == '\t'

  private def isLetter(c: Char): Boolean = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'
  private def isDigit(c: Char): Boolean = '0' <= c && c <= '9'

  private def isIdentifierStart(c: Char): Boolean = isLetter(c) || c == '_'

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Mode helpers.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def pushMode(mode: LexerMode): Unit = modes.push(mode)

  /**
    * Attempts to pop a mode off [[modes]]. If the last mode would be popped, return `false` to signal that the lexer
    * encountered an unbalanced closing token. Specific errors should be reported by the caller.
    */
  private def popMode(expectedMode: LexerMode): Boolean = {
    if (modes.length > 1) {
      assert(
        modes.top == expectedMode,
        s"Expected to pop lexer mode `$expectedMode`, encountered `${modes.top}` instead."
      )
      modes.pop()
      true
    } else false
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Input helpers.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def peek: Char = peek(1)

  private def peek(n: Int): Char = {
    val i = offset + n - 1
    if (i >= 0 && i < input.length) input.charAt(i) else EOF
  }

  private def peekNewline: Boolean = peek == '\n' || peek == '\r' && peek(2) == '\n'

  private def consume(): Char = {
    val c = peek
    if (c != EOF) offset += 1
    c
  }

  /**
    * Consumes `length` characters of the input if as many are available and returns the string. [[offset]] is only
    * mutated if the whole string is available.
    */
  private def consume(length: Int): Option[String] = {
    val endIndex = offset + length
    if (endIndex > input.length) return None
    offset += length
    input.substring(offset, endIndex).some
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
}
