package lore.compiler.parser

import lore.compiler.core.Position
import lore.compiler.syntax.Node.{NameNode, NamePathNode}
import lore.compiler.syntax.{TkDot, TkIdentifier, TkPlus}
import scalaz.Scalaz.ToOptionIdOps

import scala.collection.mutable

trait NameParser { _: Parser =>
  def name(): Option[NameNode] = consumeOnly[TkIdentifier]().map(token => NameNode(token.value, token.position))

  /**
    * A type name might be composed of several connected [[TkIdentifier]] and [[TkPlus]] tokens.
    */
  def typeName(): Option[NameNode] = {
    val connectedTokens = findConnectedTokensForTypeName()
    if (connectedTokens.isEmpty) return None

    val stringBuilder = new mutable.StringBuilder()
    connectedTokens.foreach {
      case TkIdentifier(value, _) => stringBuilder.append(value)
      case TkPlus(_) => stringBuilder.append("+")
      case _ => throw new IllegalStateException("Impossible state.")
    }

    val startIndex = connectedTokens.head.startIndex
    val endIndex = connectedTokens.last.endIndex
    NameNode(stringBuilder.toString(), Position(fragment, startIndex, endIndex)).some
  }

  // TODO (syntax): Replace element type with `TkIdentifier or TkPlus` in Scala 3.
  private def findConnectedTokensForTypeName(): Vector[PositionedToken] = {
    var connectedTokens = Vector.empty[PositionedToken]

    def addConnectedToken(): Boolean = {
      val candidate = peek match {
        case identifier: TkIdentifier => identifier
        case plus: TkPlus => plus
        case _ => return false
      }

      // Check that this token is connected to the last one.
      if (connectedTokens.nonEmpty && candidate.startIndex != connectedTokens.last.endIndex + 1) return false

      consume()
      connectedTokens :+= candidate
      true
    }

    var isConnected = true
    while (isConnected) {
      isConnected = addConnectedToken()
    }

    connectedTokens
  }

  def namePath(): Option[NamePathNode] = genericNamePath(name())

  def typeNamePath(): Option[NamePathNode] = genericNamePath(typeName())

  private def genericNamePath(name: => Option[NameNode]): Option[NamePathNode] = {
    val names = collectSep(consumeOnly(TkDot)) { name }
    if (names.isEmpty) return None
    Some(NamePathNode(names))
  }
}
