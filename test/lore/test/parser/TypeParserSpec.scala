package lore.test.parser

import lore.ast.TypeExprNode
import lore.parser.TypeParser
import lore.test.BaseSpec
import fastparse.P

class TypeParserSpec extends BaseSpec with ParserSpecExtensions[TypeExprNode] {
  import TypeExprNode._

  override def parser[_: P] = TypeParser.typeExpression

  private val A = NominalNode("A")
  private val B = NominalNode("B")
  private val C = NominalNode("C")
  private val D = NominalNode("D")
  private val E = NominalNode("E")

  "The type expression parser" should "correctly parse atomic types" in {
    "()" --> UnitNode
    "Aardvark" --> NominalNode("Aardvark")
    "(A, B)" --> ProductNode(List(A, B))
    // A single type in parentheses is unambiguously parsed as an enclosed type, not a product type.
    "(A)" --> A
    "[A]" --> ListNode(A)
    "+A" --> ComponentNode(A)
  }

  it should "correctly parse complex types" in {
    "A | B | C" --> SumNode(Set(A, B, C))
    "A -> B" --> MapNode(A, B)
    "[(A, B, C) & D & +E]" --> ListNode(IntersectionNode(Set(ProductNode(List(A, B, C)), D, ComponentNode(E))))
    "A | (B, C) | +D" --> SumNode(Set(A, ProductNode(List(B, C)), ComponentNode(D)))
    "(A, (), B)" --> ProductNode(List(A, UnitNode, B))
    "[A -> B | C -> D]" --> ListNode(SumNode(Set(MapNode(A, B), MapNode(C, D))))
  }

  it should "correctly parse type operator precedence and enclosed types" in {
    // In actual code, recommend setting some parens here, regardless of precedence.
    "A & B -> C & D | E" --> SumNode(Set(IntersectionNode(Set(A, MapNode(B, C), D)), E))
    "(A & B) -> (C & (D | E))" --> MapNode(IntersectionNode(Set(A, B)), IntersectionNode(Set(C, SumNode(Set(D, E)))))
    "A | B & C -> D" --> SumNode(Set(A, IntersectionNode(Set(B, MapNode(C, D)))))
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
