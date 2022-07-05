package lore.compiler.types

import lore.compiler.semantics.Registry
import lore.compiler.test.{BaseSpec, TypeSyntax}
import lore.compiler.utils.CollectionExtensions.OptionExtension

trait TypeSpec extends BaseSpec with TypeSyntax {
  def havingTraitSchema(name: String)(implicit registry: Registry): TraitSchema = {
    registry.rootModule.types.get(name).filterType[TraitSchema].get
  }
  def havingStructSchema(name: String)(implicit registry: Registry): StructSchema = {
    registry.rootModule.types.get(name).filterType[StructSchema].get
  }

  def havingTrait(name: String)(implicit registry: Registry): TraitType = havingTraitSchema(name).constantType
  def havingStruct(name: String)(implicit registry: Registry): StructType = havingStructSchema(name).constantType

  object TypesExample {
    implicit val registry: Registry = analyzeFragment("types/types.lore")

    val Animal = havingTrait("Animal")
    val Bird = havingTrait("Bird")
    val Mammal = havingTrait("Mammal")
    val Fish = havingTrait("Fish")
    val Chicken = havingStruct("Chicken")
    val Penguin = havingStruct("Penguin")
    val Raven = havingStruct("Raven")
    val Human = havingStruct("Human")
    val Cat = havingTrait("Cat")
    val ScottishFold = havingStruct("ScottishFold")
    val Unicorn = havingStruct("Unicorn")
    val Goldfish = havingStruct("Goldfish")
    val Koi = havingStruct("Koi")

    val Status = havingTrait("Status")
    val Healthy = havingTrait("Healthy")
    val Sick = havingTrait("Sick")

    val Zoo = havingStruct("Zoo")

    val Cage = havingTraitSchema("Cage")
    val Aquarium = havingStructSchema("Aquarium")
    val UnicornPen = havingStruct("UnicornPen")

    val ConfusedCage1 = havingTrait("ConfusedCage1")
    val ConfusedCage2 = havingTrait("ConfusedCage2")
    val ConfusedCage3 = havingTrait("ConfusedCage3")
    val ConfusedCage4 = havingTrait("ConfusedCage4")

    val Function = havingTraitSchema("Function")
    val IdentityFunction = havingStructSchema("IdentityFunction")
    val IndexFunction = havingStructSchema("IndexFunction")
    val Box = havingStructSchema("Box")
    val BoxFunction = havingStructSchema("BoxFunction")

    val Sauce = havingTrait("Sauce")
    val Ketchup = havingTrait("Ketchup")
    val Mayo = havingTrait("Mayo")

    val Steak = havingTrait("Steak")
    val Tofu = havingTrait("Tofu")

    val Meal = havingTraitSchema("Meal")
    val SurpriseMeal = havingStructSchema("SurpriseMeal")
    val MeatLoversMeal = havingStructSchema("MeatLoversMeal")
    val VeggieMeal = havingStructSchema("VeggieMeal")
    val VeganMeal = havingStruct("VeganMeal")

    val Wheel = havingTrait("Wheel")
    val CoolWheel = havingStruct("CoolWheel")
    val CheapWheel = havingStruct("CheapWheel")
    val Engine = havingTrait("Engine")
    val GasEngine = havingStruct("GasEngine")
    val ElectricEngine = havingStruct("ElectricEngine")
    val Car = havingTrait("Car")
    val Cycle = havingTrait("Cycle")
    val Motorcycle = havingStruct("Motorcycle")
    val Bicycle = havingStruct("Bicycle")

    object ShapeTypes {
      val Empty = ShapeType()

      val Position2D = ShapeType("x" -> real, "y" -> real)
      val Position3D = ShapeType("x" -> real, "y" -> real, "z" -> real)

      val Box2D = ShapeType("x" -> real, "y" -> real, "width" -> real, "height" -> real)

      val Named = ShapeType("name" -> BasicType.String)
      val Sized = ShapeType("size" -> real)
      val NamedAndSized = Named & Sized
    }
  }
}
