package lore.compiler.types

class MultipleParameterizedInheritanceSpec extends TypeSpec {

  import TypesExample._

  "The `hasMultipleParameterizedInheritance` flag" should "be correctly determined for various types" in {
    Cat.schema.hasMultipleParameterizedInheritance shouldBe false
    Goldfish.schema.hasMultipleParameterizedInheritance shouldBe false
    Aquarium.hasMultipleParameterizedInheritance shouldBe false

    ConfusedCage1.schema.hasMultipleParameterizedInheritance shouldBe false
    ConfusedCage2.schema.hasMultipleParameterizedInheritance shouldBe true
    ConfusedCage3.schema.hasMultipleParameterizedInheritance shouldBe true
    ConfusedCage4.schema.hasMultipleParameterizedInheritance shouldBe false
  }

}
