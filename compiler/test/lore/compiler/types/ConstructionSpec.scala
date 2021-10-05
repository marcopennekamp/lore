package lore.compiler.types

class ConstructionSpec extends TypeSpec {

  import TypesExample._

  "IntersectionType.construct" should "handle shape types correctly" in {
    IntersectionType.construct(
      ShapeType("x" -> BasicType.Real),
      ShapeType("y" -> BasicType.Real),
      ShapeType("z" -> BasicType.Real),
    ) shouldEqual ShapeType("x" -> BasicType.Real, "y" -> BasicType.Real, "z" -> BasicType.Real)

    IntersectionType.construct(
      ShapeType("animal" -> Chicken),
      ShapeType("animal" -> Animal, "size" -> BasicType.Int),
      ShapeType("size" -> BasicType.Real),
    ) shouldEqual ShapeType("animal" -> Chicken, "size" -> BasicType.Int)

    IntersectionType.construct(
      Cat,
      ShapeType("x" -> BasicType.Real),
      Animal,
      Chicken,
      ShapeType("y" -> BasicType.Real),
      BasicType.String,
      ShapeType("z" -> BasicType.Real),
      BasicType.Any,
      ScottishFold,
    ) shouldEqual IntersectionType(Set(
      ShapeType("x" -> BasicType.Real, "y" -> BasicType.Real, "z" -> BasicType.Real),
      ScottishFold,
      Chicken,
      BasicType.String,
    ))
  }

}
