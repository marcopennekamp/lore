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
}

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

sealed trait TkKeyword extends PositionedToken

case class TkAnd(position: TokenPosition) extends TkKeyword
case class TkCase(position: TokenPosition) extends TkKeyword
case class TkCond(position: TokenPosition) extends TkKeyword
case class TkDo(position: TokenPosition) extends TkKeyword
case class TkDomain(position: TokenPosition) extends TkKeyword
case class TkElse(position: TokenPosition) extends TkKeyword
case class TkExtends(position: TokenPosition) extends TkKeyword
case class TkFalse(position: TokenPosition) extends TkKeyword
case class TkFixed(position: TokenPosition) extends TkKeyword
case class TkFor(position: TokenPosition) extends TkKeyword
case class TkFunc(position: TokenPosition) extends TkKeyword
case class TkIf(position: TokenPosition) extends TkKeyword
case class TkIntrinsic(position: TokenPosition) extends TkKeyword
case class TkLet(position: TokenPosition) extends TkKeyword
case class TkModule(position: TokenPosition) extends TkKeyword
case class TkMut(position: TokenPosition) extends TkKeyword
case class TkNot(position: TokenPosition) extends TkKeyword
case class TkObject(position: TokenPosition) extends TkKeyword
case class TkOr(position: TokenPosition) extends TkKeyword
case class TkProc(position: TokenPosition) extends TkKeyword
case class TkReturn(position: TokenPosition) extends TkKeyword
case class TkSpec(position: TokenPosition) extends TkKeyword
case class TkStruct(position: TokenPosition) extends TkKeyword
case class TkThen(position: TokenPosition) extends TkKeyword
case class TkTrait(position: TokenPosition) extends TkKeyword
case class TkTrue(position: TokenPosition) extends TkKeyword
case class TkType(position: TokenPosition) extends TkKeyword
case class TkUse(position: TokenPosition) extends TkKeyword
case class TkVar(position: TokenPosition) extends TkKeyword
case class TkWhere(position: TokenPosition) extends TkKeyword
case class TkWhile(position: TokenPosition) extends TkKeyword
case class TkYield(position: TokenPosition) extends TkKeyword

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

case class TkInt(value: Long, position: TokenPosition) extends PositionedToken
case class TkReal(value: Double, position: TokenPosition) extends PositionedToken

// TODO (syntax): Define string tokens.

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
