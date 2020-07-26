package lore.compiler.phases.parsing.test

import fastparse.P
import lore.compiler.syntax.TypeExprNode
import lore.compiler.core.Fragment
import lore.compiler.phases.parsing.TypeParser
import lore.compiler.test.BaseSpec

class TypeParserSpec extends BaseSpec with ParserSpecExtensions[TypeExprNode] {
  import TypeExprNode._

  implicit private val fragment: Fragment = Fragment("Test", "")
  override def parser[_: P]: P[TypeExprNode] = new TypeParser().typeExpression

  private val A = NominalNode("A")
  private val B = NominalNode("B")
  private val C = NominalNode("C")
  private val D = NominalNode("D")
  private val E = NominalNode("E")

  "The type expression parser" should "correctly parse atomic types" in {
    "()" --> UnitNode()
    "Aardvark" --> NominalNode("Aardvark")
    "(A, B)" --> ProductNode(List(A, B))
    // A single type in parentheses is unambiguously parsed as an enclosed type, not a product type.
    "(A)" --> A
    "[A]" --> ListNode(A)
    "+A" --> ComponentNode(A.name)
  }

  it should "correctly parse complex types" in {
    "A | B | C" --> SumNode(List(A, B, C))
    "A -> B" --> MapNode(A, B)
    "[(A, B, C) & D & +E]" --> ListNode(IntersectionNode(List(ProductNode(List(A, B, C)), D, ComponentNode(E.name))))
    "A | (B, C) | +D" --> SumNode(List(A, ProductNode(List(B, C)), ComponentNode(D.name)))
    "(A, (), B)" --> ProductNode(List(A, UnitNode(), B))
    "[A -> B | C -> D]" --> ListNode(SumNode(List(MapNode(A, B), MapNode(C, D))))
  }

  it should "correctly parse type operator precedence and enclosed types" in {
    // In actual code, recommend setting some parens here, regardless of precedence.
    "A & B -> C & D | E" --> SumNode(List(IntersectionNode(List(A, MapNode(B, C), D)), E))
    "(A & B) -> (C & (D | E))" --> MapNode(IntersectionNode(List(A, B)), IntersectionNode(List(C, SumNode(List(D, E)))))
    "A | B & C -> D" --> SumNode(List(A, IntersectionNode(List(B, MapNode(C, D)))))
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
