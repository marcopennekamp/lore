package lore.compiler.types.test

import lore.compiler.semantics.Registry
import lore.compiler.test.{BaseSpec, TypeSyntax}
import lore.compiler.types.{StructType, TraitType}

trait TypeSpec extends BaseSpec with TypeSyntax {
  def havingTrait(name: String)(implicit registry: Registry): TraitType = registry.getType(name).get.asInstanceOf[TraitType]
  def havingStruct(name: String)(implicit registry: Registry): StructType = registry.getType(name).get.asInstanceOf[StructType]

  object TypesExample {
    implicit val registry: Registry = prepareRegistry("test/types")

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
  }
}
