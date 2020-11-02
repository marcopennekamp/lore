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
}
