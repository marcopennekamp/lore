package lore.compiler.parsing

import fastparse.P
import lore.compiler.core.Fragment
import lore.compiler.syntax.TypeExprNode

class TypeParserSpec extends ParserSpec[TypeExprNode] {
  override def parser[_: P](implicit fragment: Fragment): P[TypeExprNode] = new TypeParser(new NameParser()).typeExpression

  it should "fail on incorrect type syntax" in {
    "A & B &".fails
    "| A | B".fails
    "[A | B & [C -> [D]]".fails
    "(A, B]".fails
    "A -> (".fails
    "[A -> (B | )]".fails
    "(A, B, C,)".fails
    "(A,,B, C)".fails
  }
}
