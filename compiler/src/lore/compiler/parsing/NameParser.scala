package lore.compiler.parsing

import fastparse.{Index, P}
import lore.compiler.core.Fragment
import lore.compiler.parsing.LexicalParser.{identifier, typeIdentifier}
import lore.compiler.syntax.Node.{NameNode, NamePathNode, withPosition}

class NameParser(implicit fragment: Fragment) {
  def name[_: P]: P[NameNode] = P(Index ~~ identifier ~~ Index).map(withPosition(NameNode))
  def namePath[_: P]: P[NamePathNode] = P(genericNamePath(name))
  def structName[_: P]: P[NameNode] = P(name)
  def typeName[_: P]: P[NameNode] = P(Index ~~ typeIdentifier ~~ Index).map(withPosition(NameNode))
  def typeNamePath[_: P]: P[NamePathNode] = P(genericNamePath(typeName))
  def typeVariableName[_: P]: P[NameNode] = P(Index ~~ identifier ~~ Index).map(withPosition(NameNode))

  private def genericNamePath[_: P](name: => P[NameNode]): P[NamePathNode] = {
    P(name.repX(1, sep = ".")).map(_.toVector).map(NamePathNode(_))
  }
}
