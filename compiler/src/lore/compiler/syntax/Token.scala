package lore.compiler.syntax

import lore.compiler.syntax.Token.TokenIndex

/**
  * A token produced by the lexer.
  *
  * Token lists are generally whitespace-agnostic, as the lexer already calculates indentation levels and produces
  * [[TkIndent]] and [[TkDedent]] tokens. However, meaningful newlines are represented by [[TkNewline]] to allow the
  * parser to terminate expressions.
  */
sealed trait Token

object Token {
  type TokenIndex = Int
}

/**
  * A token with a position identified by its source index. The end index can usually be reconstructed from the type
  * and properties of the token. Because token streams are bound to a specific file, a positioned token has no
  * associated fragment. This allows the token's position to be lightweight, as compared to a full-fledged `Position`.
  */
sealed trait PositionedToken extends Token {
  /**
    * The index of the first character of the lexeme.
    */
  def startIndex: TokenIndex

  /**
    * The index <i>after</i> the last character of the lexeme.
    */
  lazy val endIndex: TokenIndex = PositionedToken.getEndIndex(this)
}

object PositionedToken {
  private def getEndIndex(token: PositionedToken): TokenIndex = token match {
    case TkIdentifier(value, position) => position + value.length
    case keyword: TkKeyword => keyword.startIndex + keyword.word.length
    case TkAnnotation(name, position) => position + 1 /* @ */ + name.length
    case TkSymbol(name, position) => position + 1 /* # */ + name.length
    case TkParenLeft(position) => position + 1
    case TkParenRight(position) => position + 1
    case TkBracketLeft(position) => position + 1
    case TkBracketRight(position) => position + 1
    case TkBraceLeft(position) => position + 1
    case TkBraceRight(position) => position + 1
    case TkShapeStart(position) => position + 2
    case TkUnderscore(position) => position + 1
    case TkPlus(position) => position + 1

    case _ =>
      throw new IllegalArgumentException(s"The end index of `$token` should have a specialized implementation.")
  }
}

/**
  * [[TkEnd]] is used by the parser to signify the end of the token stream.
  */
case object TkEnd extends Token

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Newlines and indentation.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

case object TkNewline extends Token
case object TkIndent extends Token
case object TkDedent extends Token

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
case class TkIdentifier(value: String, startIndex: TokenIndex) extends PositionedToken

sealed abstract class TkKeyword(val word: String) extends PositionedToken

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
case class TkAnnotation(name: String, startIndex: TokenIndex) extends PositionedToken

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Values.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

case class TkInt(value: Long, startIndex: TokenIndex, override val endIndex: TokenIndex) extends PositionedToken

case class TkReal(
  value: Double,
  startIndex: TokenIndex,
  override val endIndex: TokenIndex,
) extends PositionedToken

case class TkString(
  value: String,
  startIndex: TokenIndex,
  override val endIndex: TokenIndex,
) extends PositionedToken

case object TkInterpolationStart extends Token
case object TkInterpolationEnd extends Token

/**
  * A symbol `#name`, which can be a value or a type.
  */
case class TkSymbol(name: String, startIndex: TokenIndex) extends PositionedToken

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Operators and special characters.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

case class TkParenLeft(startIndex: TokenIndex) extends PositionedToken
case class TkParenRight(startIndex: TokenIndex) extends PositionedToken
case class TkBracketLeft(startIndex: TokenIndex) extends PositionedToken
case class TkBracketRight(startIndex: TokenIndex) extends PositionedToken
case class TkBraceLeft(startIndex: TokenIndex) extends PositionedToken
case class TkBraceRight(startIndex: TokenIndex) extends PositionedToken

/**
  * `%{` signifies the start of a shape value or type.
  */
case class TkShapeStart(startIndex: TokenIndex) extends PositionedToken

case object TkComma extends Token
case object TkDot extends Token
case object TkColon extends Token

case class TkUnderscore(startIndex: TokenIndex) extends PositionedToken

case object TkEquals extends Token
case object TkEqualsEquals extends Token

sealed trait TkArithmeticOperator extends Token

/**
  * [[TkPlus]] remembers its position so that type identifiers can be properly parsed.
  */
case class TkPlus(startIndex: TokenIndex) extends PositionedToken with TkArithmeticOperator

case object TkMinus extends TkArithmeticOperator
case object TkMul extends TkArithmeticOperator
case object TkDiv extends TkArithmeticOperator

case object TkPlusEquals extends Token
case object TkMinusEquals extends Token
case object TkMulEquals extends Token
case object TkDivEquals extends Token

case object TkLessThan extends Token
case object TkLessThanEquals extends Token
case object TkGreaterThan extends Token
case object TkGreaterThanEquals extends Token

case object TkTypeAnd extends Token
case object TkTypeOr extends Token

case object TkArrow extends Token
