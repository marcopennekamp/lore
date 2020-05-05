package lore.types.test

import lore.test.{BaseSpec, TypeSyntax}
import lore.types.{AnyType, ClassType, LabelType, Subtyping, Type}
import org.scalatest.Assertion

class LeastUpperBoundSpec extends BaseSpec with TypeSyntax {
  implicit val registry = prepareRegistry("test/types/lub")
  private def havingClass(name: String): ClassType = registry.getType(name).get.asInstanceOf[ClassType]
  private def havingLabel(name: String): LabelType = registry.getType(name).get.asInstanceOf[LabelType]

  private val Animal = havingClass("Animal")
  private val Bird = havingClass("Bird")
  private val Mammal = havingClass("Mammal")
  private val Fish = havingClass("Fish")
  private val Chicken = havingClass("Chicken")
  private val Penguin = havingClass("Penguin")
  private val Raven = havingClass("Raven")
  private val Human = havingClass("Human")
  private val Cat = havingClass("Cat")
  private val ScottishFold = havingClass("ScottishFold")
  private val Unicorn = havingClass("Unicorn")
  private val Goldfish = havingClass("Goldfish")

  private val Status = havingLabel("Status")
  private val Healthy = havingLabel("Healthy")
  private val Sick = havingLabel("Sick")

  private val Wheel = havingClass("Wheel")
  private val CoolWheel = havingClass("CoolWheel")
  private val CheapWheel = havingClass("CheapWheel")
  private val Engine = havingClass("Engine")
  private val GasEngine = havingClass("GasEngine")
  private val ElectricEngine = havingClass("ElectricEngine")
  private val Car = havingClass("Car")

  // TODO: Test list and map types.
  // TODO: Test sum types.
  // TODO: Test product types.

  private implicit class LubExtension(testCase: (Type, Type)) {
    val (t1, t2) = testCase
    def -->(expected: Type): Assertion = {
      Subtyping.leastUpperBound(t1, t2) shouldEqual expected
    }
  }

  "The least upper bound function" should "return the least common supertype for classes and labels" in {
    (Bird, Mammal) --> Animal
    (Fish, Bird) --> Animal
    (Fish, Animal) --> Animal
    (Fish, Healthy) --> AnyType
    (Healthy, Sick) --> Status
    (Goldfish, Human) --> Animal
    (ScottishFold, Unicorn) --> Mammal
    (Raven, Chicken) --> Bird
    (Goldfish, ScottishFold) --> Animal
  }

  it should "return the most specific supertype for intersection types" in {
    (Bird & Mammal, Animal) --> Animal
    (Bird & Fish, Mammal & Fish) --> Fish
    (Chicken & Cat, Raven & Goldfish) --> Bird
    (Chicken & Penguin, Human & Cat & Unicorn) --> Animal
    (ScottishFold & Healthy, Cat & Sick) --> (Cat & Status)
    (ScottishFold & Healthy, Goldfish & Healthy) --> (Animal & Healthy)
    (ScottishFold & Healthy, Goldfish & Sick) --> (Animal & Status)
  }

  it should "return the most specific supertype for entities, intersection types and components" in {
    (+CoolWheel & +ElectricEngine, +CheapWheel & +GasEngine) --> (+Wheel & +Engine)
    (Car, +Wheel & +Engine) --> (+Wheel & +Engine)
    (Car & +Wheel, +Wheel) --> +Wheel

    // These component types aren't related, so they cannot have a common component type as ancestor.
    (+CheapWheel, +ElectricEngine) --> AnyType

    // Problem: Car & +Wheel is a subtype of Car & +Engine, so it won't reduce to Car but Car & +Engine.
    // We should PROBABLY add a simplification step to intersection types: If an intersection type has a
    // component type and an entity type, check if the component is part of the entity; if so, remove the
    // component type. We might also have to drop AnyType from intersection types.
    // TODO: Fix this test with the above in consideration.
    (Car & +Wheel, Car & +Engine) --> Car
  }
}
