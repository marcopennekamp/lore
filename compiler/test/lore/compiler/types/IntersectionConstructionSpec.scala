package lore.compiler.types

import lore.compiler.utils.CollectionExtensions.VectorExtension
import org.scalatest.Assertion

class IntersectionConstructionSpec extends TypeSpec {

  import TypesExample._

  private implicit class SyntaxExtension(testCase: Product) {
    private val types = testCase.productIterator.toVector.filterType[Type]
    def -->(expected: Type): Assertion = {
      IntersectionType.construct(types) shouldEqual expected
    }
  }

  "IntersectionType.construct" should "handle shape types correctly" in {
    (
      ShapeType("x" -> real),
      ShapeType("y" -> real),
      ShapeType("z" -> real),
    ) --> ShapeType("x" -> real, "y" -> real, "z" -> real)

    (
      ShapeType("animal" -> Chicken),
      ShapeType("animal" -> Animal, "size" -> int),
      ShapeType("size" -> real),
    ) --> ShapeType("animal" -> Chicken, "size" -> (int & real))

    (
      Cat,
      ShapeType("x" -> real),
      Animal,
      Chicken,
      ShapeType("y" -> real),
      BasicType.String,
      ShapeType("z" -> real),
      BasicType.Any,
      ScottishFold,
    ) --> IntersectionType(Set(
      ShapeType("x" -> real, "y" -> real, "z" -> real),
      ScottishFold,
      Chicken,
      BasicType.String,
    ))
  }

}
