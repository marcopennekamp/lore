package lore.compiler.types.test

import lore.compiler.core.Registry
import lore.compiler.test.{BaseSpec, TypeSyntax}
import lore.compiler.types
import lore.compiler.types.LabelType

trait TypeSpec extends BaseSpec with TypeSyntax {
  def havingClass(name: String)(implicit registry: Registry): types.ClassType = registry.getType(name).get.asInstanceOf[types.ClassType]
  def havingLabel(name: String)(implicit registry: Registry): LabelType = registry.getType(name).get.asInstanceOf[LabelType]

  object TypesExample {
    implicit val registry: Registry = prepareRegistry("test/types")

    val Animal = havingClass("Animal")
    val Bird = havingClass("Bird")
    val Mammal = havingClass("Mammal")
    val Fish = havingClass("Fish")
    val Chicken = havingClass("Chicken")
    val Penguin = havingClass("Penguin")
    val Raven = havingClass("Raven")
    val Human = havingClass("Human")
    val Cat = havingClass("Cat")
    val ScottishFold = havingClass("ScottishFold")
    val Unicorn = havingClass("Unicorn")
    val Goldfish = havingClass("Goldfish")

    val Status = havingLabel("Status")
    val Healthy = havingLabel("Healthy")
    val Sick = havingLabel("Sick")

    val Wheel = havingClass("Wheel")
    val CoolWheel = havingClass("CoolWheel")
    val CheapWheel = havingClass("CheapWheel")
    val Engine = havingClass("Engine")
    val GasEngine = havingClass("GasEngine")
    val ElectricEngine = havingClass("ElectricEngine")
    val Car = havingClass("Car")
    val Cycle = havingClass("Cycle")
    val Motorcycle = havingClass("Motorcycle")
    val Bicycle = havingClass("Bicycle")
  }
}
