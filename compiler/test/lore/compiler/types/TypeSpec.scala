package lore.compiler.types

import lore.compiler.semantics.Registry
import lore.compiler.test.{BaseSpec, TypeSyntax}

trait TypeSpec extends BaseSpec with TypeSyntax {
  def havingTraitSchema(name: String)(implicit registry: Registry): TraitSchema = registry.typeScope.getTraitSchema(name).get
  def havingStructSchema(name: String)(implicit registry: Registry): StructSchema = registry.typeScope.getStructSchema(name).get

  def instantiateSchema(schema: TypeSchema, arguments: Vector[Type]): Type = schema.instantiate(arguments).get

  def havingTrait(name: String)(implicit registry: Registry): TraitType = havingTraitSchema(name).representative
  def havingStruct(name: String)(implicit registry: Registry): StructType = havingStructSchema(name).representative

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

    val Zoo = havingStruct("Zoo")

    val Cage = havingTraitSchema("Cage")
    val Aquarium = havingStructSchema("Aquarium")
    val UnicornPen = havingStruct("UnicornPen")

    val ConfusedCage1 = havingTrait("ConfusedCage1")
    val ConfusedCage2 = havingTrait("ConfusedCage2")
    val ConfusedCage3 = havingTrait("ConfusedCage3")
    val ConfusedCage4 = havingTrait("ConfusedCage4")

    val Status = havingTrait("Status")
    val Healthy = havingTrait("Healthy")
    val Sick = havingTrait("Sick")

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

      val Position2D = ShapeType("x" -> BasicType.Real, "y" -> BasicType.Real)
      val Position3D = ShapeType("x" -> BasicType.Real, "y" -> BasicType.Real, "z" -> BasicType.Real)

      val Box2D = ShapeType("x" -> BasicType.Int, "y" -> BasicType.Int, "width" -> BasicType.Int, "height" -> BasicType.Int)

      val Named = ShapeType("name" -> BasicType.String)
      val Sized = ShapeType("size" -> BasicType.Real)
      val NamedAndSized = Named & Sized
    }
  }
}
