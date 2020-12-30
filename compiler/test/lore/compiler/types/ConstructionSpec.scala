package lore.compiler.types

class ConstructionSpec extends TypeSpec {

  import TypesExample._

  // TODO: Test SumType.construct.

  "IntersectionType.construct" should "handle shape types correctly" in {
    IntersectionType.construct(Vector(
      shape("x" -> BasicType.Real),
      shape("y" -> BasicType.Real),
      shape("z" -> BasicType.Real),
    )) shouldEqual shape("x" -> BasicType.Real, "y" -> BasicType.Real, "z" -> BasicType.Real)

    IntersectionType.construct(Vector(
      shape("animal" -> Chicken),
      shape("animal" -> Animal, "size" -> BasicType.Int),
      shape("size" -> BasicType.Real),
    )) shouldEqual shape("animal" -> Chicken, "size" -> BasicType.Int)

    IntersectionType.construct(Vector(
      Cat,
      shape("x" -> BasicType.Real),
      Animal,
      Chicken,
      shape("y" -> BasicType.Real),
      BasicType.String,
      shape("z" -> BasicType.Real),
      BasicType.Any,
      ScottishFold,
    )) shouldEqual IntersectionType(Set(
      shape("x" -> BasicType.Real, "y" -> BasicType.Real, "z" -> BasicType.Real),
      ScottishFold,
      Chicken,
      BasicType.String,
    ))
  }

}
