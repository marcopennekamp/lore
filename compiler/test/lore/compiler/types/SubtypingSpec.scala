package lore.compiler.types

import org.scalatest.Assertion

class SubtypingSpec extends TypeSpec {
  import TypesExample._

  private implicit class TypeExtension(t1: Type) {
    def <:<(t2: Type): Assertion = assert(t1 <= t2)
    def </<(t2: Type): Assertion = assert(t1 </= t2)
  }

  "Subtyping" should "handle sum types correctly" in {
    ((string | int) | boolean) <:< (string | int | boolean)
    (((string | int) | boolean) | int) <:< (string | int | boolean)
  }

  it should "handle shape types correctly" in {
    import ShapeTypes._

    Empty <:< Empty

    Position3D <:< Position2D
    Position2D </< Position3D
    Empty </< Position2D
    Empty </< Position3D
    Position2D <:< Empty
    Position3D <:< Empty

    Box2D <:< Position2D
    Box2D </< Position3D
    Position2D </< Box2D
    Position3D </< Box2D
    Empty </< Box2D
    Box2D <:< Empty

    Named </< Sized
    Sized </< Named
    Goldfish <:< Named
    Named </< Goldfish
    Goldfish <:< Sized
    Sized </< Goldfish
    Goldfish <:< NamedAndSized
    NamedAndSized </< Goldfish
    Goldfish <:< Empty
    Empty </< Goldfish
  }

  it should "handle type variables correctly" in {
    { // An excerpt of Example 1 from the spec's type allocation examples.
      val C = typeVariable("C")
      int </< C
    }
    { // An excerpt of Example 2 from the spec's type allocation examples.
      val C = typeVariable("C")
      Cat </< C
      C </< Animal
    }
    { // An excerpt of Example 3 from the spec's type allocation examples.
      val C = typeVariable("C", Goldfish, Fish)
      val D = typeVariable("D", Goldfish, C)
      Goldfish <:< C
      C <:< Animal
      BasicType.Nothing <:< D
      D <:< C
    }
  }

  it should "handle traits and structs with type arguments correctly" in {
    val animalCage = Cage(Animal)
    val birdCage = Cage(Bird)
    val mammalCage = Cage(Mammal)
    val fishCage = Cage(Fish)
    val fishAquarium = Aquarium(Fish)
    val goldfishCage = Cage(Goldfish)
    val goldfishAquarium = Aquarium(Goldfish)
    val koiAquarium = Aquarium(Koi)

    fishCage <:< animalCage
    animalCage </< fishCage

    fishAquarium <:< animalCage
    animalCage </< fishAquarium
    fishAquarium </< birdCage
    fishAquarium </< mammalCage
    fishAquarium <:< fishCage
    fishCage </< fishAquarium

    goldfishCage </< fishAquarium
    fishAquarium </< goldfishCage

    goldfishAquarium <:< fishAquarium
    fishAquarium </< goldfishAquarium
    goldfishAquarium <:< goldfishCage

    koiAquarium <:< fishAquarium
    fishAquarium </< koiAquarium
    goldfishAquarium </< koiAquarium
    koiAquarium </< goldfishAquarium

    UnicornPen <:< animalCage
    UnicornPen </< birdCage
    UnicornPen <:< mammalCage
    UnicornPen </< fishCage
  }
}
