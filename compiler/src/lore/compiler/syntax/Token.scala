package lore.compiler.syntax

import lore.compiler.syntax.Token.TokenIndex

/**
  * A token produced by the lexer.
  *
  * Token lists are generally whitespace-agnostic, as the lexer already calculates indentation levels and produces
  * [[TkIndent]] and [[TkDedent]] tokens. However, meaningful newlines are represented by [[TkNewline]] to allow the
  * parser to terminate expressions.
  *
  * A token's position is identified by its source index. The end index can usually be reconstructed from the type and
  * properties of the token. Because token streams are bound to a specific file, a token has no associated fragment.
  * This allows the token's position to be lightweight, as compared to a full-fledged `Position`.
  */
sealed trait Token {
  /**
    * The index of the first character of the lexeme.
    */
  def startIndex: TokenIndex

  /**
    * The index <i>after</i> the last character of the lexeme.
    */
  lazy val endIndex: TokenIndex = Token.getEndIndex(this)

  def isControlToken: Boolean = this match {
    case _: TkNewline | _: TkIndent | _: TkDedent => true
    case _ => false
  }
}

object Token {
  // TODO (syntax): Merge with `Node.index`/`Position.index`.
  type TokenIndex = Int

  private def getEndIndex(token: Token): TokenIndex = token match {
    case TkIdentifier(value, startIndex) => startIndex + value.length
    case keyword: TkKeyword => keyword.startIndex + keyword.word.length
    case TkAnnotation(name, startIndex) => startIndex + 1 /* @ */ + name.length
    case TkSymbol(name, startIndex) => startIndex + 1 /* # */ + name.length
    case TkShapeStart(startIndex) => startIndex + 2
    case TkEqualsEquals(startIndex) => startIndex + 2
    case TkPlusEquals(startIndex) => startIndex + 2
    case TkMinusEquals(startIndex) => startIndex + 2
    case TkMulEquals(startIndex) => startIndex + 2
    case TkDivEquals(startIndex) => startIndex + 2
    case TkLessThanEquals(startIndex) => startIndex + 2
    case TkGreaterThanEquals(startIndex) => startIndex + 2
    case TkTypeLessThan(startIndex) => startIndex + 2
    case TkTypeGreaterThan(startIndex) => startIndex + 2
    case TkArrow(startIndex) => startIndex + 2
    case _ => token.startIndex + 1
  }
}

/**
  * [[TkEnd]] is used by the parser to signify the end of the token stream.
  */
case class TkEnd(startIndex: TokenIndex) extends Token

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Newlines and indentation.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

case class TkNewline(startIndex: TokenIndex) extends Token
case class TkIndent(startIndex: TokenIndex) extends Token
case class TkDedent(startIndex: TokenIndex) extends Token

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Identifiers and keywords.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
  * A possibly incomplete identifier. The lexer can consume `?` and `!` eagerly because these characters are valid in
  * all kinds of identifiers, but a `+` in the case of type identifiers cannot be consumed eagerly, because the lexer
  * cannot differentiate between type and expression contexts. Hence, a type identifier will possibly be tokenized into
  * multiple [[TkIdentifier]] and [[TkPlus]] tokens. Type identifier reconstruction involves checking the adjacency of
  * [[TkIdentifier]] and [[TkPlus]] via their indices.
  *
  * Name paths cannot be tokenized fully for the same reason, as a type name path may contain a `+` character.
  */
case class TkIdentifier(value: String, startIndex: TokenIndex) extends Token

sealed abstract class TkKeyword(val word: String) extends Token

// TODO (syntax): Rename keyword classes to `TkKeyword*` or `TkKw*`.
case class TkAnd(startIndex: TokenIndex) extends TkKeyword("and")
case class TkCase(startIndex: TokenIndex) extends TkKeyword("case")
case class TkCond(startIndex: TokenIndex) extends TkKeyword("cond")
case class TkDo(startIndex: TokenIndex) extends TkKeyword("do")
case class TkDomain(startIndex: TokenIndex) extends TkKeyword("domain")
case class TkElse(startIndex: TokenIndex) extends TkKeyword("else")
case class TkExtends(startIndex: TokenIndex) extends TkKeyword("extends")
case class TkFalse(startIndex: TokenIndex) extends TkKeyword("false")
case class TkFixed(startIndex: TokenIndex) extends TkKeyword("fixed")
case class TkFor(startIndex: TokenIndex) extends TkKeyword("for")
case class TkFunc(startIndex: TokenIndex) extends TkKeyword("func")
case class TkIf(startIndex: TokenIndex) extends TkKeyword("if")
case class TkIntrinsic(startIndex: TokenIndex) extends TkKeyword("intrinsic")
case class TkLet(startIndex: TokenIndex) extends TkKeyword("let")
case class TkModule(startIndex: TokenIndex) extends TkKeyword("module")
case class TkMut(startIndex: TokenIndex) extends TkKeyword("mut")
case class TkNot(startIndex: TokenIndex) extends TkKeyword("not")
case class TkObject(startIndex: TokenIndex) extends TkKeyword("object")
// TODO (syntax): `open` is a very awkward keyword. For example, `file.open`. Consider changing it or converting it to a
//                soft keyword. (This should be quite possible for `open`, considering its current use cases.)
case class TkOpen(startIndex: TokenIndex) extends TkKeyword("open")
case class TkOr(startIndex: TokenIndex) extends TkKeyword("or")
case class TkProc(startIndex: TokenIndex) extends TkKeyword("proc")
case class TkReturn(startIndex: TokenIndex) extends TkKeyword("return")
case class TkSpec(startIndex: TokenIndex) extends TkKeyword("spec")
case class TkStruct(startIndex: TokenIndex) extends TkKeyword("struct")
case class TkThen(startIndex: TokenIndex) extends TkKeyword("then")
case class TkTrait(startIndex: TokenIndex) extends TkKeyword("trait")
case class TkTrue(startIndex: TokenIndex) extends TkKeyword("true")
case class TkType(startIndex: TokenIndex) extends TkKeyword("type")
case class TkUse(startIndex: TokenIndex) extends TkKeyword("use")
case class TkVar(startIndex: TokenIndex) extends TkKeyword("var")
case class TkWhere(startIndex: TokenIndex) extends TkKeyword("where")
case class TkWhile(startIndex: TokenIndex) extends TkKeyword("while")
case class TkYield(startIndex: TokenIndex) extends TkKeyword("yield")

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Annotations.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
  * An annotation `@name`. Note that despite [[TkWhere]] being a keyword, the lexer allows the name `where` to tokenize
  * an `@where`.
  */
case class TkAnnotation(name: String, startIndex: TokenIndex) extends Token

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Values.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

case class TkInt(value: Long, startIndex: TokenIndex, override val endIndex: TokenIndex) extends Token

case class TkReal(
  value: Double,
  startIndex: TokenIndex,
  override val endIndex: TokenIndex,
) extends Token

case class TkString(
  value: String,
  startIndex: TokenIndex,
  override val endIndex: TokenIndex,
) extends Token

case class TkInterpolationStart(startIndex: TokenIndex, override val endIndex: TokenIndex) extends Token
case class TkInterpolationEnd(startIndex: TokenIndex) extends Token

/**
  * A symbol `#name`, which can be a value or a type.
  */
case class TkSymbol(name: String, startIndex: TokenIndex) extends Token

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Operators and special characters.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

case class TkParenLeft(startIndex: TokenIndex) extends Token
case class TkParenRight(startIndex: TokenIndex) extends Token
case class TkBracketLeft(startIndex: TokenIndex) extends Token
case class TkBracketRight(startIndex: TokenIndex) extends Token
case class TkBraceLeft(startIndex: TokenIndex) extends Token
case class TkBraceRight(startIndex: TokenIndex) extends Token

/**
  * `%{` signifies the start of a shape value or type.
  */
case class TkShapeStart(startIndex: TokenIndex) extends Token

case class TkComma(startIndex: TokenIndex) extends Token
case class TkDot(startIndex: TokenIndex) extends Token
case class TkColon(startIndex: TokenIndex) extends Token

case class TkUnderscore(startIndex: TokenIndex) extends Token

case class TkEquals(startIndex: TokenIndex) extends Token
case class TkEqualsEquals(startIndex: TokenIndex) extends Token

sealed trait TkArithmeticOperator extends Token

/**
  * [[TkPlus]] remembers its position so that type identifiers can be properly parsed.
  */
case class TkPlus(startIndex: TokenIndex) extends Token with TkArithmeticOperator

case class TkMinus(startIndex: TokenIndex) extends TkArithmeticOperator
case class TkMul(startIndex: TokenIndex) extends TkArithmeticOperator
case class TkDiv(startIndex: TokenIndex) extends TkArithmeticOperator

case class TkPlusEquals(startIndex: TokenIndex) extends Token
case class TkMinusEquals(startIndex: TokenIndex) extends Token
case class TkMulEquals(startIndex: TokenIndex) extends Token
case class TkDivEquals(startIndex: TokenIndex) extends Token

case class TkLessThan(startIndex: TokenIndex) extends Token
case class TkLessThanEquals(startIndex: TokenIndex) extends Token
case class TkGreaterThan(startIndex: TokenIndex) extends Token
case class TkGreaterThanEquals(startIndex: TokenIndex) extends Token

case class TkTypeAnd(startIndex: TokenIndex) extends Token
case class TkTypeOr(startIndex: TokenIndex) extends Token
case class TkTypeLessThan(startIndex: TokenIndex) extends Token
case class TkTypeGreaterThan(startIndex: TokenIndex) extends Token

case class TkArrow(startIndex: TokenIndex) extends Token
