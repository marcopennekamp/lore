package lore.compiler.parser

import lore.compiler.syntax.{ExprNode, TkInterpolationEnd, TkInterpolationStart, TkString}
import lore.compiler.syntax.ExprNode.{ConcatenationNode, StringLiteralNode}

trait ExpressionParser { _: Parser =>
  def parseExpression(): Result[ExprNode] = ???

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Strings.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def stringExpression(): Result[ExprNode] = {
    val startPosition = peek.position

    val expressions = collectLookahead(peekIs[TkString] || peekIs[TkInterpolationStart]) {
      peek match {
        case _: TkString => stringLiteral()
        case _: TkInterpolationStart => stringInterpolation()
        case _ =>
          // TODO (syntax): Report error: String literal or string interpolation expected.
          Failure
      }
    }.getOrElse(return Failure)

    expressions match {
      case Vector() =>
        // TODO (syntax): Report error: String expected, but neither literal nor interpolation found.
        Failure

      case Vector(expression) => expression.success
      case _ => ConcatenationNode(expressions, startPosition.to(expressions.last.position)).success
    }
  }

  private def stringLiteral(): Result[StringLiteralNode] =
    consumeExpect[TkString].map {
      token => StringLiteralNode(token.value, token.position)
    }

  private def stringInterpolation(): Result[ExprNode] = {
    consumeExpect[TkInterpolationStart].getOrElse(return Failure)

    // We do not need to care about whether the tokens for the expression contain newlines, because the lexer will never
    // generate an interpolation that contains newlines.
    val expression = parseExpression().getOrElse(return Failure)

    consume[TkInterpolationEnd].getOrElse {
      // TODO (syntax): Report error: End of interpolation expected.
      return Failure
    }

    expression.success
  }
}
