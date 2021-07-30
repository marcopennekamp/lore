package lore.compiler.types

import org.scalatest.Assertion

class DirectSubtypesSpec extends TypeSpec {

  import TypesExample._

  private def assertDirectSubtypes(dt: DeclaredType)(expected: DeclaredType*): Assertion = {
    registry.declaredTypeHierarchy.getDirectSubtypes(dt).toSet shouldEqual expected.toSet
  }

  private def assertConcreteSubtypes(dt: DeclaredType)(expected: DeclaredType*): Assertion = {
    registry.declaredTypeHierarchy.getConcreteSubtypes(dt).toSet shouldEqual expected.toSet
  }

  "DeclaredTypeHierarchy.getDirectSubtypes" should "compute the correct subtypes for parameterized traits" in {
    assertDirectSubtypes(Cage(Animal))(
      Aquarium(Fish), UnicornPen, ConfusedCage1, ConfusedCage2, ConfusedCage3, ConfusedCage4
    )
    assertDirectSubtypes(Cage(Fish))(
      Aquarium(Fish), ConfusedCage1, ConfusedCage4
    )
    assertDirectSubtypes(Cage(Unicorn))(
      UnicornPen, ConfusedCage2, ConfusedCage3
    )
  }

  "DeclaredTypeHierarchy.getConcreteSubtypes" should "compute the correct concrete subtypes for parameterized traits" in {
    // TODO (schemas): Write some tests for DeclaredTypeHierarchy.getConcreteSubtypes.
  }

}
