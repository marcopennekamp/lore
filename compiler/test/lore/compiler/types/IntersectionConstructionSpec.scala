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
      ShapeType("x" -> BasicType.Real),
      ShapeType("y" -> BasicType.Real),
      ShapeType("z" -> BasicType.Real),
    ) --> ShapeType("x" -> BasicType.Real, "y" -> BasicType.Real, "z" -> BasicType.Real)

    (
      ShapeType("animal" -> Chicken),
      ShapeType("animal" -> Animal, "size" -> BasicType.Int),
      ShapeType("size" -> BasicType.Real),
    ) --> ShapeType("animal" -> Chicken, "size" -> BasicType.Int)

    (
      Cat,
      ShapeType("x" -> BasicType.Real),
      Animal,
      Chicken,
      ShapeType("y" -> BasicType.Real),
      BasicType.String,
      ShapeType("z" -> BasicType.Real),
      BasicType.Any,
      ScottishFold,
    ) --> IntersectionType(Set(
      ShapeType("x" -> BasicType.Real, "y" -> BasicType.Real, "z" -> BasicType.Real),
      ScottishFold,
      Chicken,
      BasicType.String,
    ))
  }

}
