package lore.compiler.types

import org.scalatest.Assertion

class LeastUpperBoundSpec extends TypeSpec {
  import TypesExample._

  private implicit class LubExtension(testCase: (Type, Type)) {
    val (t1, t2) = testCase
    def -->(expected: Type): Assertion = {
      LeastUpperBound.leastUpperBound(t1, t2) shouldEqual expected
    }
  }

  "The least upper bound function" should "return the most specific supertype for traits and structs" in {
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
    (BasicType.Int | BasicType.Real, BasicType.Nothing) --> BasicType.Real
  }

  it should "return the most specific supertype for lists and maps" in {
    (ListType(Bird), ListType(Mammal)) --> ListType(Animal)
    (ListType(Bird & Fish), ListType(Mammal & Fish)) --> ListType(Fish)
    (MapType(BasicType.String, Human), MapType(BasicType.Int, Unicorn)) --> MapType(BasicType.String | BasicType.Int, Mammal)
    (MapType(BasicType.Real, Cat & Healthy), MapType(BasicType.Int, ScottishFold & Sick)) --> MapType(BasicType.Real, Cat & Status)
  }

  it should "return the most specific supertype for shape types and structs" in {
    (Goldfish, ShapeTypes.Named) --> ShapeTypes.Named
    (ShapeTypes.Sized, Goldfish) --> ShapeTypes.Sized
    (ShapeTypes.Sized, ShapeTypes.Named) --> ShapeTypes.Empty
    (shape("animal" -> (Cat | Penguin)), shape("animal" -> Raven)) --> shape("animal" -> Animal)
  }

  it should "return the correct least upper bounds for type variables" in {
    val A = new TypeVariable("A", BasicType.Nothing, Bird, 0)
    val B = new TypeVariable("B", BasicType.Nothing, Mammal, 1)
    val C = new TypeVariable("C", BasicType.Nothing, Wheel, 2)
    (A, Chicken) --> Bird
    (Chicken, A) --> Bird
    (B, Human) --> Mammal
    (Human, B) --> Mammal
    (A, ScottishFold) --> Animal
    (ScottishFold, A) --> Animal
    (B, ScottishFold | Unicorn) --> Mammal
    (B, A) --> Animal
    (A, B) --> Animal
    (A | B, Bird) --> Animal
    (A, B | Bird) --> (B | Bird)
    (A, BasicType.String) --> (A | BasicType.String)
    (BasicType.String, A) --> (A | BasicType.String)
    (A, C) --> (A | C)
    (C, A) --> (A | C)
    (B, C) --> (B | C)
    (C, B) --> (B | C)
    (A | B, C) --> (A | B | C)
    (A | B, A | C) --> (A | B | C)
    (A & B, C) --> ((A & B) | C)
  }
}
