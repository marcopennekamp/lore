package lore.types.test

import lore.compiler.Registry
import lore.test.{BaseSpec, TypeSyntax}
import lore.types._
import org.scalatest.Assertion

class LeastUpperBoundSpec extends BaseSpec with TypeSyntax {
  implicit val registry: Registry = prepareRegistry("test/types/lub")
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

  private implicit class LubExtension(testCase: (Type, Type)) {
    val (t1, t2) = testCase
    def -->(expected: Type): Assertion = {
      Subtyping.leastUpperBound(t1, t2) shouldEqual expected
    }
  }

  "The least upper bound function" should "return the most specific supertype for classes and labels" in {
    (Bird, Mammal) --> Animal
    (Fish, Bird) --> Animal
    (Fish, Animal) --> Animal
    (Fish, Healthy) --> (Fish | Healthy)
    (Healthy, Sick) --> Status
    (Goldfish, Human) --> Animal
    (ScottishFold, Unicorn) --> Mammal
    (Raven, Chicken) --> Bird
    (Goldfish, ScottishFold) --> Animal
  }

  it should "return the most specific supertype for product types" in {
    ((Bird, Goldfish): Type, (Raven, Chicken): Type) --> (Bird, Animal)
    ((Penguin, Chicken, Raven): Type, (Chicken, Raven, Penguin): Type) --> (Bird, Bird, Bird)
    ((Chicken & Penguin, Goldfish & Cat, Unicorn & Healthy): Type, (Raven & Sick, Fish & Sick, Human & Sick): Type) -->
      ((Bird, Fish, Mammal & Status))
    ((+Wheel, +Engine): Type, (Car, ElectricEngine): Type) --> (+Wheel, +Engine | ElectricEngine)
  }

  it should "return the most specific supertype for intersection types" in {
    (Bird & Mammal, Animal) --> Animal
    (Bird & Fish, Mammal & Fish) --> Fish
    (Chicken & Cat, Raven & Goldfish) --> Bird
    (Chicken & Penguin, Human & Cat & Unicorn) --> Animal
    ((Cat | Penguin) & Healthy, Raven & Sick) --> (Animal & Status)
    (ScottishFold & Healthy, Cat & Sick) --> (Cat & Status)
    (ScottishFold & Healthy, Goldfish & Healthy) --> (Animal & Healthy)
    (ScottishFold & Healthy, Goldfish & Sick) --> (Animal & Status)
  }

  it should "return the most specific supertype for sum types" in {
    (Chicken | Raven, Penguin | Chicken) --> Bird
    (Chicken | Cat | Goldfish, Human | Unicorn) --> Animal
    (Human | (ScottishFold & Healthy), Unicorn | (Cat & Sick)) --> Mammal
    // This doesn't really test the LUB, but it does ensure that sum types are simplified at some point,
    // either in the LUB or in the sum type construction itself. Where exactly doesn't matter for the
    // test.
    (BasicType.Int | BasicType.Real, NothingType) --> BasicType.Real
  }

  it should "return the most specific supertype for entities, intersection types and components" in {
    (+CoolWheel & +ElectricEngine, +CheapWheel & +GasEngine) --> (+Wheel & +Engine)
    (Car, +Wheel & +Engine) --> (+Wheel & +Engine)
    (Car & +Wheel, +Wheel) --> +Wheel
    // These component types aren't related, so they cannot have a common component type as ancestor.
    (+CheapWheel, +ElectricEngine) --> (+CheapWheel | +ElectricEngine)
    (Car & +Wheel, Car & +Engine) --> Car
  }

  it should "return the most specific supertype for lists and maps" in {
    (ListType(Bird), ListType(Mammal)) --> ListType(Animal)
    (ListType(Bird & Fish), ListType(Mammal & Fish)) --> ListType(Fish)
    (MapType(BasicType.String, Human), MapType(BasicType.Int, Unicorn)) --> MapType(BasicType.String | BasicType.Int, Mammal)
    (MapType(BasicType.Real, Cat & Healthy), MapType(BasicType.Int, ScottishFold & Sick)) --> MapType(BasicType.Real, Cat & Status)
  }
}
