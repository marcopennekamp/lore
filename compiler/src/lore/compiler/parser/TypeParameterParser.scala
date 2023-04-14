package lore.compiler.parser

import lore.compiler.core.Position
import lore.compiler.syntax.DeclNode.TypeVariableNode
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.syntax._
import scalaz.Scalaz.ToOptionIdOps

trait TypeParameterParser { _: Parser with TypeParser with NameParser =>
  def simpleTypeParameter(): Result[TypeVariableNode] =
    parseTypeParameterGiven(Variance.Invariant, isOpen = false, startPosition = peek.position)

  def traitTypeParameter(): Result[TypeVariableNode] = {
    val startPosition = peek.position
    val variance = optionalVariance()
    parseTypeParameterGiven(variance, isOpen = false, startPosition)
  }

  def structTypeParameter(): Result[TypeVariableNode] = {
    val startPosition = peek.position

    // We could require covariance ("+") for open parameters here, but the parser shouldn't report this kind of semantic
    // error. Hence, it's better to explicitly check this in a later phase.
    val isOpen = consumeIf[TkOpen]
    val variance = optionalVariance()
    parseTypeParameterGiven(variance, isOpen, startPosition)
  }

  private def parseTypeParameterGiven(
    variance: Variance,
    isOpen: Boolean,
    startPosition: Position,
  ): Result[TypeVariableNode] = {
    val name = typeVariableName().getOrElse {
      // TODO (syntax): Report error: Type variable name expected.
      return Failure
    }

    val lowerBound = if (consumeIf[TkTypeGreaterThan]) {
      typeExpression().getOrElse(return Failure).some
    } else None

    val upperBound = if (consumeIf[TkTypeLessThan]) {
      typeExpression().getOrElse(return Failure).some
    } else None

    val position = startPosition.toEither(upperBound, lowerBound, name)
    TypeVariableNode(name, lowerBound, upperBound, variance, isOpen, position).success
  }

  private def optionalVariance(): Variance = peek match {
    case _: TkPlus =>
      skip()
      Variance.Covariant

    case _: TkMinus =>
      skip()
      Variance.Contravariant

    case _ => Variance.Invariant
  }
}
