package lore.compiler.syntax

import lore.compiler.syntax.Token.TokenPosition

/**
  * A token produced by the lexer.
  *
  * Token lists are generally whitespace-agnostic, as the lexer already calculates indentation levels and produces
  * [[TkIndent]] and [[TkDedent]] tokens. However, meaningful newlines are represented by [[TkNewline]] to allow the
  * parser to terminate expressions.
  */
sealed trait Token

object Token {
  type TokenPosition = Int
}

/**
  * A token with a position identified by its source index. The end position can be reconstructed from the type and
  * contents of the token. Because token streams are bound to a specific file, a positioned token has no associated
  * fragment.
  */
sealed trait PositionedToken extends Token {
  def position: TokenPosition

  lazy val endPosition: TokenPosition = PositionedToken.getEndPosition(this)
}

object PositionedToken {
  private def getEndPosition(token: PositionedToken): TokenPosition = token match {
    case TkIdentifier(value, position) => position + value.length
    case keyword: TkKeyword => keyword.position + keyword.word.length
    case TkAnnotation(name, position) => position + 1 /* @ */ + name.length
    case TkSymbol(name, position) => position + 1 /* # */ + name.length
    case TkParenLeft(position) => position + 1
    case TkParenRight(position) => position + 1
    case TkBracketLeft(position) => position + 1
    case TkBracketRight(position) => position + 1
    case TkBraceLeft(position) => position + 1
    case TkBraceRight(position) => position + 1
    case TkShapeStart(position) => position + 2
    case TkDot(position) => position + 1
    case TkUnderscore(position) => position + 1
    case TkPlus(position) => position + 1

    case _ =>
      throw new IllegalArgumentException(s"The end position for `$token` should have a specialized implementation.")
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
case class TkIdentifier(value: String, position: TokenPosition) extends PositionedToken

sealed abstract class TkKeyword(val word: String) extends PositionedToken

case class TkAnd(position: TokenPosition) extends TkKeyword("and")
case class TkCase(position: TokenPosition) extends TkKeyword("case")
case class TkCond(position: TokenPosition) extends TkKeyword("cond")
case class TkDo(position: TokenPosition) extends TkKeyword("do")
case class TkDomain(position: TokenPosition) extends TkKeyword("domain")
case class TkElse(position: TokenPosition) extends TkKeyword("else")
case class TkExtends(position: TokenPosition) extends TkKeyword("extends")
case class TkFalse(position: TokenPosition) extends TkKeyword("false")
case class TkFixed(position: TokenPosition) extends TkKeyword("fixed")
case class TkFor(position: TokenPosition) extends TkKeyword("for")
case class TkFunc(position: TokenPosition) extends TkKeyword("func")
case class TkIf(position: TokenPosition) extends TkKeyword("if")
case class TkIntrinsic(position: TokenPosition) extends TkKeyword("intrinsic")
case class TkLet(position: TokenPosition) extends TkKeyword("let")
case class TkModule(position: TokenPosition) extends TkKeyword("module")
case class TkMut(position: TokenPosition) extends TkKeyword("mut")
case class TkNot(position: TokenPosition) extends TkKeyword("not")
case class TkObject(position: TokenPosition) extends TkKeyword("object")
case class TkOr(position: TokenPosition) extends TkKeyword("or")
case class TkProc(position: TokenPosition) extends TkKeyword("proc")
case class TkReturn(position: TokenPosition) extends TkKeyword("return")
case class TkSpec(position: TokenPosition) extends TkKeyword("spec")
case class TkStruct(position: TokenPosition) extends TkKeyword("struct")
case class TkThen(position: TokenPosition) extends TkKeyword("then")
case class TkTrait(position: TokenPosition) extends TkKeyword("trait")
case class TkTrue(position: TokenPosition) extends TkKeyword("true")
case class TkType(position: TokenPosition) extends TkKeyword("type")
case class TkUse(position: TokenPosition) extends TkKeyword("use")
case class TkVar(position: TokenPosition) extends TkKeyword("var")
case class TkWhere(position: TokenPosition) extends TkKeyword("where")
case class TkWhile(position: TokenPosition) extends TkKeyword("while")
case class TkYield(position: TokenPosition) extends TkKeyword("yield")

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Annotations.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
  * An annotation `@name`. Note that despite [[TkWhere]] being a keyword, the lexer allows the name `where` to tokenize
  * an `@where`.
  */
case class TkAnnotation(name: String, position: TokenPosition) extends PositionedToken

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Values.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

case class TkInt(value: Long, position: TokenPosition, override val endPosition: TokenPosition) extends PositionedToken

case class TkReal(
  value: Double,
  position: TokenPosition,
  override val endPosition: TokenPosition,
) extends PositionedToken

case class TkString(
  value: String,
  position: TokenPosition,
  override val endPosition: TokenPosition,
) extends PositionedToken

case object TkInterpolationStart extends Token
case object TkInterpolationEnd extends Token

/**
  * A symbol `#name`, which can be a value or a type.
  */
case class TkSymbol(name: String, position: TokenPosition) extends PositionedToken

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Operators and special characters.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

case class TkParenLeft(position: TokenPosition) extends PositionedToken
case class TkParenRight(position: TokenPosition) extends PositionedToken
case class TkBracketLeft(position: TokenPosition) extends PositionedToken
case class TkBracketRight(position: TokenPosition) extends PositionedToken
case class TkBraceLeft(position: TokenPosition) extends PositionedToken
case class TkBraceRight(position: TokenPosition) extends PositionedToken

/**
  * `%{` signifies the start of a shape value or type.
  */
case class TkShapeStart(position: TokenPosition) extends PositionedToken

case object TkComma extends Token

/**
  * [[TkDot]] remembers its position so that name paths can be properly parsed.
  */
case class TkDot(position: TokenPosition) extends PositionedToken

case object TkColon extends Token
case class TkUnderscore(position: TokenPosition) extends PositionedToken

case object TkEquals extends Token
case object TkEqualsEquals extends Token

sealed trait TkArithmeticOperator extends Token

/**
  * [[TkPlus]] remembers its position so that type identifiers can be properly parsed.
  */
case class TkPlus(position: TokenPosition) extends PositionedToken with TkArithmeticOperator

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
