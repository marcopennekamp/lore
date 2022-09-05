package lore.compiler.types

import org.scalatest.Assertion

class FitSpec extends TypeSpec {

  import TypesExample._

  private implicit class TypeExtension(t1: Type) {
    def fitsInto(t2: Type): Assertion = assert(t1 fits t2)
    def fitsNotInto(t2: Type): Assertion = assert(t1 fitsNot t2)
  }

  "Fit" should "handle type variables correctly" in {
    { val A = typeVariable("A", 0)
      ((ListType(string), string): TupleType) fitsInto (ListType(A), A)
      ((ListType(real), string): TupleType) fitsNotInto (ListType(A), A)
    }
    { val X = typeVariable("X", 0)
      val Y = typeVariable("Y", 1)
      ((real, Y): TupleType) fitsInto (X, BasicType.Any)
    }
    { val A = typeVariable("A", 0)
      val B = typeVariable("B", 1, upperBound = A)
      ((ListType(Animal), Bird): TupleType) fitsInto (ListType(A), B)
      ((ListType(Bird), Mammal): TupleType) fitsNotInto (ListType(A), B)
    }
    { // Example 1 from the spec's type allocation examples.
      val A = typeVariable("A", 0)
      val B = typeVariable("B", 1, upperBound = A)
      val C = typeVariable("C", 2)
      ((C, int): TupleType) fitsNotInto (A, B)
    }
    { // Example 2 from the spec's type allocation examples.
      val A = typeVariable("A", 0, Cat, Animal)
      val B = typeVariable("B", 1, upperBound = A)
      val C = typeVariable("C", 2)
      ((C, Cat): TupleType) fitsNotInto (A, B)
    }
    { // Example 3 from the spec's type allocation examples.
      val A = typeVariable("A", 0, Cat, Animal)
      val B = typeVariable("B", 1, upperBound = A)
      val C = typeVariable("C", 2, Cat, Mammal)
      val D = typeVariable("D", 3, ScottishFold, C)
      ((C, D): TupleType) fitsInto (A, B)
    }
    { val A = typeVariable("A", 0)
      val B = typeVariable("B", 1, lowerBound = A)
      ((ListType(string | int), int): TupleType) fitsInto (ListType(B), A)
    }
  }

  it should "handle shape types and structs correctly" in {
    import ShapeTypes._

    { val A = typeVariable("A", 0)
      Goldfish fitsInto ShapeType("name" -> A)
      Goldfish fitsInto ShapeType("name" -> A, "size" -> real)
      Goldfish fitsInto ShapeType("name" -> string, "size" -> A)
      Goldfish fitsNotInto ShapeType("name" -> A, "size" -> A)
    }
    { val X = typeVariable("X", 0, upperBound = real)
      val Y = typeVariable("Y", 1, upperBound = real)
      val Z = typeVariable("Z", 2, upperBound = real)
      val T2D = ShapeType("x" -> X, "y" -> Y)
      val T3D = T2D & ShapeType("z" -> Z)
      Position2D fitsInto T2D
      Position2D fitsNotInto T3D
      Position3D fitsInto T2D
      Position3D fitsInto T3D
      Box2D fitsInto T2D
      Box2D fitsNotInto T3D
    }
    { val A = typeVariable("A", 0)
      Zoo fitsInto ShapeType("animals" -> ListType(A))
    }
  }

  it should "handle traits and structs with type parameters correctly" in {
    val A = typeVariable("A", 0, upperBound = Animal)
    val B = typeVariable("B", 1, upperBound = Fish)
    val C = typeVariable("C", 2, upperBound = Fish)

    val cage = Cage(A)
    val fishCage = Cage(B)
    val aquarium = Aquarium(C)

    val fishAquarium = Aquarium(Fish)
    val goldfishCage = Cage(Goldfish)
    val goldfishAquarium = Aquarium(Goldfish)
    val koiAquarium = Aquarium(Koi)
    val catCage = Cage(Cat)

    fishCage fitsInto cage
    fishCage fitsNotInto aquarium
    aquarium fitsInto cage
    aquarium fitsInto fishCage

    fishAquarium fitsInto cage
    fishAquarium fitsInto fishCage
    fishAquarium fitsInto aquarium
    goldfishCage fitsInto cage
    goldfishCage fitsInto fishCage
    goldfishCage fitsNotInto aquarium
    goldfishAquarium fitsInto cage
    goldfishAquarium fitsInto fishCage
    goldfishAquarium fitsInto aquarium
    koiAquarium fitsInto cage
    koiAquarium fitsInto fishCage
    koiAquarium fitsInto aquarium

    catCage fitsInto cage
    catCage fitsNotInto fishCage
    catCage fitsNotInto aquarium

    UnicornPen fitsInto cage
    UnicornPen fitsNotInto fishCage
    UnicornPen fitsNotInto aquarium

    ConfusedCage1 fitsInto cage
    ConfusedCage1 fitsInto fishCage
    ConfusedCage1 fitsNotInto aquarium
    ConfusedCage2 fitsInto cage
    ConfusedCage2 fitsInto fishCage
    ConfusedCage2 fitsNotInto aquarium
    ConfusedCage3 fitsInto cage
    ConfusedCage3 fitsInto fishCage
    ConfusedCage3 fitsNotInto aquarium
  }

}
