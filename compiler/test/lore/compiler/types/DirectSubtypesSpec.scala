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
      UnicornPen, ConfusedCage3
    )

    assertDirectSubtypes(Function(string, Box(string)))(BoxFunction(string))
    assertDirectSubtypes(Function(int, int))(IdentityFunction(int), IndexFunction(int))
    assertDirectSubtypes(Function(int, string))(IndexFunction(string))
    assertDirectSubtypes(Function(string, string))(IdentityFunction(string))
    assertDirectSubtypes(Function(Fish, Box(Fish)))(BoxFunction(Fish))
    assertDirectSubtypes(Function(Animal, Box(Fish)))()

    assertDirectSubtypes(Meal(Sauce, Steak))(
      SurpriseMeal(Sauce, Steak), MeatLoversMeal(Sauce)
    )
    assertDirectSubtypes(Meal(Ketchup, Steak))(
      SurpriseMeal(Ketchup, Steak), MeatLoversMeal(Ketchup)
    )
    assertDirectSubtypes(Meal(Mayo, Steak))(
      SurpriseMeal(Mayo, Steak), MeatLoversMeal(Mayo)
    )
    assertDirectSubtypes(Meal(Sauce, Tofu))(
      SurpriseMeal(Sauce, Tofu), VeggieMeal(Sauce), VeganMeal
    )
    assertDirectSubtypes(Meal(Ketchup, Tofu))(
      SurpriseMeal(Ketchup, Tofu), VeggieMeal(Ketchup), VeganMeal
    )
    assertDirectSubtypes(Meal(Mayo, Tofu))(
      SurpriseMeal(Mayo, Tofu), VeggieMeal(Mayo)
    )
  }

  "DeclaredTypeHierarchy.getConcreteSubtypes" should "compute the correct concrete subtypes for parameterized traits" in {
    // TODO (schemas): Write some tests for DeclaredTypeHierarchy.getConcreteSubtypes.
  }

}