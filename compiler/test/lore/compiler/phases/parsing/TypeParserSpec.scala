package lore.compiler.phases.parsing

import fastparse.P
import lore.compiler.core.Fragment
import lore.compiler.syntax.TypeExprNode
import lore.compiler.test.BaseSpec

// TODO: Implement these tests using functional tests. We cannot check the produced AST directly, then, but we can
//       produce values that will only be assignable to a variable of a given type if the type is parsed correctly.

class TypeParserSpec extends BaseSpec with ParserSpecExtensions[TypeExprNode] {
  override def parser[_: P](implicit fragment: Fragment): P[TypeExprNode] = new TypeParser().typeExpression

  import TestNodes._

  private val A = Type.Identifier("A")
  private val B = Type.Identifier("B")
  private val C = Type.Identifier("C")
  private val D = Type.Identifier("D")
  private val E = Type.Identifier("E")

  "The type expression parser" should "correctly parse atomic types" in {
    "()" --> Type.Unit()
    "Aardvark" --> Type.Identifier("Aardvark")
    "(A, B)" --> Type.Tuple(Vector(A, B))
    // A single type in parentheses is unambiguously parsed as an enclosed type, not a tuple type.
    "(A)" --> A
    "[A]" --> Type.List(A)
  }

  it should "correctly parse complex types" in {
    "A | B | C" --> Type.Sum(Vector(A, B, C))
    "A -> B" --> Type.Map(A, B)
    "[(A, B, C) & D & E]" --> Type.List(Type.Intersection(Vector(Type.Tuple(Vector(A, B, C)), D, E)))
    "A | (B, C) | D" --> Type.Sum(Vector(A, Type.Tuple(Vector(B, C)), D))
    "(A, (), B)" --> Type.Tuple(Vector(A, Type.Unit(), B))
    "[A -> B | C -> D]" --> Type.List(Type.Sum(Vector(Type.Map(A, B), Type.Map(C, D))))
  }

  it should "correctly parse type operator precedence and enclosed types" in {
    "A & B -> C & D | E" --> Type.Sum(Vector(Type.Intersection(Vector(A, Type.Map(B, C), D)), E))
    "(A & B) -> (C & (D | E))" --> Type.Map(Type.Intersection(Vector(A, B)), Type.Intersection(Vector(C, Type.Sum(Vector(D, E)))))
    "A | B & C -> D" --> Type.Sum(Vector(A, Type.Intersection(Vector(B, Type.Map(C, D)))))
  }

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
