package lore.compiler.types

import org.scalatest.Assertion

class FitSpec extends TypeSpec {

  import TypesExample._

  private implicit class TypeExtension(t1: Type) {
    def fitsInto(t2: Type): Assertion = assert(Fit.fits(t1, t2))
    def fitsNotInto(t2: Type): Assertion = assert(!Fit.fits(t1, t2))
  }

  "Fit" should "handle type variables correctly" in {
    { val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, 0)
      ((ListType(BasicType.String), BasicType.String): ProductType) fitsInto (ListType(A), A)
      ((ListType(BasicType.Real), BasicType.String): ProductType) fitsNotInto (ListType(A), A)
      ((ListType(BasicType.Real), BasicType.Int): ProductType) fitsNotInto (ListType(A), A)
    }
    { val X = new TypeVariable("X", BasicType.Nothing, BasicType.Any, 0)
      val Y = new TypeVariable("Y", BasicType.Nothing, BasicType.Real, 1)
      ((BasicType.Real, Y): ProductType) fitsInto (X, BasicType.Any)
    }
    { val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, 0)
      val B = new TypeVariable("B", BasicType.Nothing, A, 1)
      ((ListType(BasicType.Real), BasicType.Int): ProductType) fitsInto (ListType(A), B)
      ((ListType(Bird), Mammal): ProductType) fitsNotInto (ListType(A), B)
    }
    { // Example 1 from the spec's type allocation examples.
      val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, 0)
      val B = new TypeVariable("B", BasicType.Nothing, A, 1)
      val C = new TypeVariable("C", BasicType.Nothing, BasicType.Any, 2)
      ((C, BasicType.Int): ProductType) fitsNotInto (A, B)
    }
    { // Example 2 from the spec's type allocation examples.
      val A = new TypeVariable("A", Cat, Animal, 0)
      val B = new TypeVariable("B", BasicType.Nothing, A, 1)
      val C = new TypeVariable("C", BasicType.Nothing, BasicType.Any, 2)
      ((C, Cat): ProductType) fitsNotInto (A, B)
    }
    { // Example 3 from the spec's type allocation examples.
      val A = new TypeVariable("A", Cat, Animal, 0)
      val B = new TypeVariable("B", BasicType.Nothing, A, 1)
      val C = new TypeVariable("C", Cat, Mammal, 2)
      val D = new TypeVariable("D", ScottishFold, C, 3)
      ((C, D): ProductType) fitsInto (A, B)
    }
    { val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, 0)
      val B = new TypeVariable("B", A, BasicType.Any, 1)
      ((ListType(BasicType.String | BasicType.Int), BasicType.Int): ProductType) fitsInto (ListType(B), A)
    }
  }

  it should "handle shape types and structs correctly" in {
    import ShapeTypes._

    { val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, 0)
      Goldfish fitsInto shape("name" -> A)
      Goldfish fitsInto shape("name" -> A, "size" -> BasicType.Real)
      Goldfish fitsInto shape("name" -> BasicType.String, "size" -> A)
      Goldfish fitsNotInto shape("name" -> A, "size" -> A)
    }
    { val X = new TypeVariable("X", BasicType.Nothing, BasicType.Real, 0)
      val Y = new TypeVariable("Y", BasicType.Nothing, BasicType.Real, 1)
      val Z = new TypeVariable("Z", BasicType.Nothing, BasicType.Real, 2)
      val T2D = shape("x" -> X, "y" -> Y)
      //val T3D = T2D & shape("z" -> Z)   TODO: Use this once intersection types combine shape types during simplification.
      val T3D = shape("x" -> X, "y" -> Y, "z" -> Z)
      Position2D fitsInto T2D
      Position2D fitsNotInto T3D
      Position3D fitsInto T2D
      Position3D fitsInto T3D
      Box2D fitsInto T2D
      Box2D fitsNotInto T3D
    }
    { val A = new TypeVariable("A", BasicType.Nothing, BasicType.Any, 0)
      // TODO: The test with B currently does not work. See the TODO in TypeVariableAllocation.of.
      //val B = new TypeVariable("B", BasicType.Nothing, ListType(A), 1)
      //Zoo fitsInto shape("animals" -> B)

      Zoo fitsInto shape("animals" -> ListType(A))
    }
  }

}
