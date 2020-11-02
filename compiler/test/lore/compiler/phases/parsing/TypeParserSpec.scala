package lore.compiler.phases.parsing

import fastparse.P
import lore.compiler.syntax.TypeExprNode
import lore.compiler.core.Fragment
import lore.compiler.test.BaseSpec

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
    "(A, B)" --> Type.Product(Vector(A, B))
    // A single type in parentheses is unambiguously parsed as an enclosed type, not a product type.
    "(A)" --> A
    "[A]" --> Type.List(A)
    "+A" --> Type.Component(A.name)
  }

  it should "correctly parse complex types" in {
    "A | B | C" --> Type.Sum(Vector(A, B, C))
    "A -> B" --> Type.Map(A, B)
    "[(A, B, C) & D & +E]" --> Type.List(Type.Intersection(Vector(Type.Product(Vector(A, B, C)), D, Type.Component(E.name))))
    "A | (B, C) | +D" --> Type.Sum(Vector(A, Type.Product(Vector(B, C)), Type.Component(D.name)))
    "(A, (), B)" --> Type.Product(Vector(A, Type.Unit(), B))
    "[A -> B | C -> D]" --> Type.List(Type.Sum(Vector(Type.Map(A, B), Type.Map(C, D))))
  }

  it should "correctly parse type operator precedence and enclosed types" in {
    // In actual code, recommend setting some parens here, regardless of precedence.
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
