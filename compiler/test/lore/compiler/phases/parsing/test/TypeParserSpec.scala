package lore.compiler.phases.parsing.test

import fastparse.P
import lore.compiler.syntax.TypeExprNode
import lore.compiler.core.Fragment
import lore.compiler.phases.parsing.TypeParser
import lore.compiler.test.BaseSpec

class TypeParserSpec extends BaseSpec with ParserSpecExtensions[TypeExprNode] {
  import TestNodes._

  implicit private val fragment: Fragment = Fragment("Test", "")
  override def parser[_: P]: P[TypeExprNode] = new TypeParser().typeExpression

  private val A = Type.Nominal("A")
  private val B = Type.Nominal("B")
  private val C = Type.Nominal("C")
  private val D = Type.Nominal("D")
  private val E = Type.Nominal("E")

  "The type expression parser" should "correctly parse atomic types" in {
    "()" --> Type.Unit()
    "Aardvark" --> Type.Nominal("Aardvark")
    "(A, B)" --> Type.Product(List(A, B))
    // A single type in parentheses is unambiguously parsed as an enclosed type, not a product type.
    "(A)" --> A
    "[A]" --> Type.List(A)
    "+A" --> Type.Component(A.name)
  }

  it should "correctly parse complex types" in {
    "A | B | C" --> Type.Sum(List(A, B, C))
    "A -> B" --> Type.Map(A, B)
    "[(A, B, C) & D & +E]" --> Type.List(Type.Intersection(List(Type.Product(List(A, B, C)), D, Type.Component(E.name))))
    "A | (B, C) | +D" --> Type.Sum(List(A, Type.Product(List(B, C)), Type.Component(D.name)))
    "(A, (), B)" --> Type.Product(List(A, Type.Unit(), B))
    "[A -> B | C -> D]" --> Type.List(Type.Sum(List(Type.Map(A, B), Type.Map(C, D))))
  }

  it should "correctly parse type operator precedence and enclosed types" in {
    // In actual code, recommend setting some parens here, regardless of precedence.
    "A & B -> C & D | E" --> Type.Sum(List(Type.Intersection(List(A, Type.Map(B, C), D)), E))
    "(A & B) -> (C & (D | E))" --> Type.Map(Type.Intersection(List(A, B)), Type.Intersection(List(C, Type.Sum(List(D, E)))))
    "A | B & C -> D" --> Type.Sum(List(A, Type.Intersection(List(B, Type.Map(C, D)))))
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
