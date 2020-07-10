package lore.compiler.types.test

import lore.compiler.types.{AnyType, BasicType, Fit, ListType, NothingType, ProductType, Type, TypeVariable}
import org.scalatest.Assertion

class FitSpec extends TypeSpec {
  import TypesExample._

  private implicit class TypeExtension(t1: Type) {
    def fitsInto(t2: Type): Assertion = assert(Fit.fits(t1, t2))
    def fitsNotInto(t2: Type): Assertion = assert(!Fit.fits(t1, t2))
  }

  "Fit" should "handle type variables correctly" in {
    { val A = new TypeVariable("A", NothingType, AnyType, 0)
      ((ListType(BasicType.String), BasicType.String): ProductType) fitsInto (ListType(A), A)
      ((ListType(BasicType.Real), BasicType.String): ProductType) fitsNotInto (ListType(A), A)
      ((ListType(BasicType.Real), BasicType.Int): ProductType) fitsNotInto (ListType(A), A)
    }
    { val X = new TypeVariable("X", NothingType, AnyType, 0)
      val Y = new TypeVariable("Y", NothingType, BasicType.Real, 1)
      ((BasicType.Real, Y): ProductType) fitsInto (X, AnyType)
    }
    { val A = new TypeVariable("A", NothingType, AnyType, 0)
      val B = new TypeVariable("B", NothingType, A, 1)
      ((ListType(BasicType.Real), BasicType.Int): ProductType) fitsInto (ListType(A), B)
      ((ListType(Bird), Mammal): ProductType) fitsNotInto (ListType(A), B)
    }
    { // Example 1 from the spec's type allocation examples.
      val A = new TypeVariable("A", NothingType, AnyType, 0)
      val B = new TypeVariable("B", NothingType, A, 1)
      val C = new TypeVariable("C", NothingType, AnyType, 2)
      ((C, BasicType.Int): ProductType) fitsNotInto (A, B)
    }
    { // Example 2 from the spec's type allocation examples.
      val A = new TypeVariable("A", Cat, Animal, 0)
      val B = new TypeVariable("B", NothingType, A, 1)
      val C = new TypeVariable("C", NothingType, AnyType, 2)
      ((C, Cat): ProductType) fitsNotInto (A, B)
    }
    { // Example 3 from the spec's type allocation examples.
      val A = new TypeVariable("A", Cat, Animal, 0)
      val B = new TypeVariable("B", NothingType, A, 1)
      val C = new TypeVariable("C", Cat, Mammal, 2)
      val D = new TypeVariable("D", ScottishFold, C, 3)
      ((C, D): ProductType) fitsInto (A, B)
    }
    { val A = new TypeVariable("A", NothingType, AnyType, 0)
      val B = new TypeVariable("B", A, AnyType, 1)
      ((ListType(BasicType.String | BasicType.Int), BasicType.Int): ProductType) fitsInto (ListType(B), A)
    }
  }
}
