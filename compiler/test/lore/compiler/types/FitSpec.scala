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
    { val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, Variance.Invariant)
      ((ListType(BasicType.String), BasicType.String): TupleType) fitsInto (ListType(A), A)
      ((ListType(BasicType.Real), BasicType.String): TupleType) fitsNotInto (ListType(A), A)
      ((ListType(BasicType.Real), BasicType.Int): TupleType) fitsNotInto (ListType(A), A)
    }
    { val X = new TypeVariable("X", BasicType.Nothing, BasicType.Any, Variance.Invariant)
      val Y = new TypeVariable("Y", BasicType.Nothing, BasicType.Real, Variance.Invariant)
      ((BasicType.Real, Y): TupleType) fitsInto (X, BasicType.Any)
    }
    { val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, Variance.Invariant)
      val B = new TypeVariable("B", BasicType.Nothing, A, Variance.Invariant)
      ((ListType(BasicType.Real), BasicType.Int): TupleType) fitsInto (ListType(A), B)
      ((ListType(Bird), Mammal): TupleType) fitsNotInto (ListType(A), B)
    }
    { // Example 1 from the spec's type allocation examples.
      val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, Variance.Invariant)
      val B = new TypeVariable("B", BasicType.Nothing, A, Variance.Invariant)
      val C = new TypeVariable("C", BasicType.Nothing, BasicType.Any, Variance.Invariant)
      ((C, BasicType.Int): TupleType) fitsNotInto (A, B)
    }
    { // Example 2 from the spec's type allocation examples.
      val A = new TypeVariable("A", Cat, Animal, Variance.Invariant)
      val B = new TypeVariable("B", BasicType.Nothing, A, Variance.Invariant)
      val C = new TypeVariable("C", BasicType.Nothing, BasicType.Any, Variance.Invariant)
      ((C, Cat): TupleType) fitsNotInto (A, B)
    }
    { // Example 3 from the spec's type allocation examples.
      val A = new TypeVariable("A", Cat, Animal, Variance.Invariant)
      val B = new TypeVariable("B", BasicType.Nothing, A, Variance.Invariant)
      val C = new TypeVariable("C", Cat, Mammal, Variance.Invariant)
      val D = new TypeVariable("D", ScottishFold, C, Variance.Invariant)
      ((C, D): TupleType) fitsInto (A, B)
    }
    { val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, Variance.Invariant)
      val B = new TypeVariable("B", A, BasicType.Any, Variance.Invariant)
      ((ListType(BasicType.String | BasicType.Int), BasicType.Int): TupleType) fitsInto (ListType(B), A)
    }
  }

  it should "handle shape types and structs correctly" in {
    import ShapeTypes._

    { val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, Variance.Invariant)
      Goldfish fitsInto ShapeType("name" -> A)
      Goldfish fitsInto ShapeType("name" -> A, "size" -> BasicType.Real)
      Goldfish fitsInto ShapeType("name" -> BasicType.String, "size" -> A)
      Goldfish fitsNotInto ShapeType("name" -> A, "size" -> A)
    }
    { val X = new TypeVariable("X", BasicType.Nothing, BasicType.Real, Variance.Invariant)
      val Y = new TypeVariable("Y", BasicType.Nothing, BasicType.Real, Variance.Invariant)
      val Z = new TypeVariable("Z", BasicType.Nothing, BasicType.Real, Variance.Invariant)
      val T2D = ShapeType("x" -> X, "y" -> Y)
      val T3D = T2D & ShapeType("z" -> Z)
      Position2D fitsInto T2D
      Position2D fitsNotInto T3D
      Position3D fitsInto T2D
      Position3D fitsInto T3D
      Box2D fitsInto T2D
      Box2D fitsNotInto T3D
    }
    { val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, Variance.Invariant)
      // TODO: The test with B currently does not work. See the TODO in TypeVariableAllocation.of.
      //val B = new TypeVariable("B", BasicType.Nothing, ListType(A), 1)
      //Zoo fitsInto ShapeType("animals" -> B)

      Zoo fitsInto ShapeType("animals" -> ListType(A))
    }
  }

  it should "handle traits and structs with type parameters correctly" in {
    val A = new TypeVariable("A", BasicType.Nothing, Animal, Variance.Invariant)
    val B = new TypeVariable("B", BasicType.Nothing, Fish, Variance.Invariant)
    val C = new TypeVariable("C", BasicType.Nothing, Fish, Variance.Invariant)

    val cage = instantiateSchema(Cage, Vector(A))
    val fishCage = instantiateSchema(Cage, Vector(B))
    val aquarium = instantiateSchema(Aquarium, Vector(C))

    val fishAquarium = instantiateSchema(Aquarium, Vector(Fish))
    val goldfishCage = instantiateSchema(Cage, Vector(Goldfish))
    val goldfishAquarium = instantiateSchema(Aquarium, Vector(Goldfish))
    val koiAquarium = instantiateSchema(Aquarium, Vector(Koi))
    val catCage = instantiateSchema(Cage, Vector(Cat))

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
    // TODO (schemas): The following two checks come down to the subtyping `ConfusedCage3 <= Cage[Fish & Unicorn]`,
    //                 which is currently resolved incorrectly because we're checking the supertypes `Cage[Fish]` and
    //                 `Cage[Unicorn]` of ConfusedCage3 separately.
    //ConfusedCage3 fitsInto cage
    //ConfusedCage3 fitsInto fishCage
    ConfusedCage3 fitsNotInto aquarium
  }

}
