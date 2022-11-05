package lore.compiler.parser

import lore.compiler.syntax.Node.{NameNode, NamePathNode}

import scala.collection.immutable.HashSet
import scala.collection.mutable

trait NameParser { _: Parser =>
  private val keywords: Set[String] = HashSet(
    "_", "and", "cond", "do", "domain", "else", "extends", "false", "fixed", "for", "func", "if", "intrinsic", "let",
    "module", "mut", "not", "object", "or", "proc", "return", "spec", "struct", "then", "trait", "true", "type", "use",
    "var", "where", "while", "case", "yield",
  )

  def name(): Option[NameNode] = withPosition(identifier()).map(NameNode.tupled)

  def namePath(): Option[NamePathNode] = genericNamePath(name())

  def typeName(): Option[NameNode] = withPosition(typeIdentifier()).map(NameNode.tupled)

  def typeNamePath(): Option[NamePathNode] = genericNamePath(typeName())

  private def genericNamePath(name: => Option[NameNode]): Option[NamePathNode] = {
    val names = collectSepChar('.') { name }
    if (names.isEmpty) return None
    Some(NamePathNode(names))
  }

  def identifier(): Option[String] = genericIdentifier(_ => false)

  def typeIdentifier(): Option[String] = genericIdentifier(c => c == '+')

  // TODO (syntax): Should be inline (Scala 3).
  private def genericIdentifier(allowFirst: Char => Boolean): Option[String] = {
    val firstChar = peek
    if (!isLetter(firstChar) && firstChar != '_' && !allowFirst(firstChar)) return None
    consume()

    val builder = new mutable.StringBuilder()
    builder.append(firstChar)

    charsWhile(
      c => isLetter(c) || isDigit(c) || c == '_' || c == '!' || c == '?',
      builder.append(_),
    )

    val id = builder.toString()
    if (!keywords.contains(id)) Some(id)
    else None
  }
}
