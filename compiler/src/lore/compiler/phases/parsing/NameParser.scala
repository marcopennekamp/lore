package lore.compiler.phases.parsing

import fastparse.{Index, P}
import lore.compiler.core.Fragment
import lore.compiler.phases.parsing.LexicalParser.{identifier, typeIdentifier}
import lore.compiler.syntax.Node.{NameNode, withPosition}

class NameParser(implicit fragment: Fragment) {
  def name[_: P]: P[NameNode] = P(Index ~~ identifier ~~ Index).map(withPosition(NameNode))
  def structName[_: P]: P[NameNode] = P(name)
  def typeName[_: P]: P[NameNode] = P(Index ~~ typeIdentifier ~~ Index).map(withPosition(NameNode))
  def typeVariableName[_: P]: P[NameNode] = P(Index ~~ identifier ~~ Index).map(withPosition(NameNode))
}
