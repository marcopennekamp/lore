package lore.compiler.types

import lore.compiler.types.TypeVariable.Variance
import org.scalatest.Assertion

class FitSpec extends TypeSpec {

  import TypesExample._

  private implicit class TypeExtension(t1: Type) {
    def fitsInto(t2: Type): Assertion = assert(Fit.fits(t1, t2))
    def fitsNotInto(t2: Type): Assertion = assert(!Fit.fits(t1, t2))
  }

  "Fit" should "handle type variables correctly" in {
    { val A = typeVariable("A")
      ((ListType(BasicType.String), BasicType.String): TupleType) fitsInto (ListType(A), A)
      ((ListType(BasicType.Real), BasicType.String): TupleType) fitsNotInto (ListType(A), A)
      ((ListType(BasicType.Real), BasicType.Int): TupleType) fitsNotInto (ListType(A), A)
    }
    { val X = typeVariable("X")
      val Y = typeVariable("Y")
      ((BasicType.Real, Y): TupleType) fitsInto (X, BasicType.Any)
    }
    { val A = typeVariable("A")
      val B = typeVariable("B")
      ((ListType(BasicType.Real), BasicType.Int): TupleType) fitsInto (ListType(A), B)
      ((ListType(Bird), Mammal): TupleType) fitsNotInto (ListType(A), B)
    }
    { // Example 1 from the spec's type allocation examples.
      val A = typeVariable("A")
      val B = typeVariable("B")
      val C = typeVariable("C")
      ((C, BasicType.Int): TupleType) fitsNotInto (A, B)
    }
    { // Example 2 from the spec's type allocation examples.
      val A = typeVariable("A", Cat, Animal)
      val B = typeVariable("B", upperBound = A)
      val C = typeVariable("C")
      ((C, Cat): TupleType) fitsNotInto (A, B)
    }
    { // Example 3 from the spec's type allocation examples.
      val A = typeVariable("A", Cat, Animal)
      val B = typeVariable("B", upperBound = A)
      val C = typeVariable("C", Cat, Mammal)
      val D = typeVariable("D", ScottishFold, C)
      ((C, D): TupleType) fitsInto (A, B)
    }
    { val A = typeVariable("A")
      val B = typeVariable("B", lowerBound = A)
      ((ListType(BasicType.String | BasicType.Int), BasicType.Int): TupleType) fitsInto (ListType(B), A)
    }
  }

  it should "handle shape types and structs correctly" in {
    import ShapeTypes._

    { val A = typeVariable("A")
      Goldfish fitsInto ShapeType("name" -> A)
      Goldfish fitsInto ShapeType("name" -> A, "size" -> BasicType.Real)
      Goldfish fitsInto ShapeType("name" -> BasicType.String, "size" -> A)
      Goldfish fitsNotInto ShapeType("name" -> A, "size" -> A)
    }
    { val X = typeVariable("X", upperBound = BasicType.Real)
      val Y = typeVariable("Y", upperBound = BasicType.Real)
      val Z = typeVariable("Z", upperBound = BasicType.Real)
      val T2D = ShapeType("x" -> X, "y" -> Y)
      val T3D = T2D & ShapeType("z" -> Z)
      Position2D fitsInto T2D
      Position2D fitsNotInto T3D
      Position3D fitsInto T2D
      Position3D fitsInto T3D
      Box2D fitsInto T2D
      Box2D fitsNotInto T3D
    }
    { val A = typeVariable("A")
      // TODO: The test with B currently does not work. See the TODO in TypeVariableAllocation.of.
      //val B = typeVariable("B", upperBound = ListType(A))
      //Zoo fitsInto ShapeType("animals" -> B)

      Zoo fitsInto ShapeType("animals" -> ListType(A))
    }
  }

  it should "handle traits and structs with type parameters correctly" in {
    val A = typeVariable("A", upperBound = Animal)
    val B = typeVariable("B", upperBound = Fish)
    val C = typeVariable("C", upperBound = Fish)

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
