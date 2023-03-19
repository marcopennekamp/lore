package lore.compiler.parser

import lore.compiler.core.Position
import lore.compiler.syntax.Node.{NameNode, NamePathNode}
import lore.compiler.syntax.{TkDot, TkIdentifier, TkPlus, Token}

import scala.collection.mutable

/**
  * Callees are expected to report an error if a name parser fails.
  */
trait NameParser { _: Parser =>
  def name(): Result[NameNode] = consume[TkIdentifier].map(token => NameNode(token.value, token.position))

  /**
    * A type name might be composed of several connected [[TkIdentifier]] and [[TkPlus]] tokens.
    */
  def typeName(): Result[NameNode] = {
    val connectedTokens = collectConnectedTokens(isTypeNameStart)
    if (connectedTokens.isEmpty) return Failure

    val stringBuilder = new mutable.StringBuilder()
    connectedTokens.foreach {
      case TkIdentifier(value, _) => stringBuilder.append(value)
      case TkPlus(_) => stringBuilder.append("+")
      case _ => throw new IllegalStateException("Impossible state.")
    }

    val startIndex = connectedTokens.head.startIndex
    val endIndex = connectedTokens.last.endIndex
    NameNode(stringBuilder.toString(), Position(fragment, startIndex, endIndex)).success
  }

  def typeVariableName(): Result[NameNode] = name()

  def namePath(): Result[NamePathNode] = genericNamePath(name())

  def typeNamePath(): Result[NamePathNode] = genericNamePath(typeName())

  def isTypeNameStart(token: Token): Boolean = token match {
    case _: TkIdentifier | _: TkPlus => true
    case _ => false
  }

  private def genericNamePath(name: => Result[NameNode]): Result[NamePathNode] = {
    val names = collectSepBacktrack(consumeIf[TkDot]) { name }
    if (names.isEmpty) return Failure
    NamePathNode(names).success
  }
}
