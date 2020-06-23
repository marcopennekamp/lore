package lore.compiler.types.test

import lore.types.{AnyType, Assignability, BasicType, ListType, NothingType, ProductType, Type, TypeVariable}
import org.scalatest.Assertion

class FitSpec extends TypeSpec {
  import TypesExample._

  private implicit class TypeExtension(t1: Type) {
    def fitsInto(t2: Type): Assertion = assert(Assignability.fits(t1, t2))
    def fitsNotInto(t2: Type): Assertion = assert(!Assignability.fits(t1, t2))
  }

  "Fit" should "handle type variables correctly" in {
    { val A = new TypeVariable("A", NothingType, AnyType)
      // ([String], String) is assignable to ([A], A) where A <: Any
      ((ListType(BasicType.String), BasicType.String): ProductType) fitsInto (ListType(A), A)
      // ([Real], String) is NOT assignable to ([A], A) where A <: Any
      ((ListType(BasicType.Real), BasicType.String): ProductType) fitsNotInto (ListType(A), A)
      // ([Real], Int) is NOT assignable to ([A], A) where A <: Any
      ((ListType(BasicType.Real), BasicType.Int): ProductType) fitsNotInto (ListType(A), A)
    }
    { val X = new TypeVariable("X", NothingType, AnyType)
      val Y = new TypeVariable("Y", NothingType, BasicType.Real)
      // (Real, Y) is assignable to (X, Any) where X <: Any, Y <: Real
      ((BasicType.Real, Y): ProductType) fitsInto (X, AnyType)
    }
    { val A = new TypeVariable("A", NothingType, AnyType)
      val B = new TypeVariable("B", NothingType, A)
      // ([Real], Int) is assignable to ([A], B) where A <: Any, B <: A
      ((ListType(BasicType.Real), BasicType.Int): ProductType) fitsInto (ListType(A), B)
      // ([T1], T2) is NOT assignable to ([A], B <: A) where A <: Any, B <: A
      ((ListType(Bird), Mammal): ProductType) fitsNotInto (ListType(A), B)
    }
    { // Example 1 from the spec's type allocation examples.
      val A = new TypeVariable("A", NothingType, AnyType)
      val B = new TypeVariable("B", NothingType, A)
      val C = new TypeVariable("C", NothingType, AnyType)
      ((C, BasicType.Int): ProductType) fitsNotInto (A, B)
    }
    { // Example 2 from the spec's type allocation examples.
      val A = new TypeVariable("A", Cat, Animal)
      val B = new TypeVariable("B", NothingType, A)
      val C = new TypeVariable("C", NothingType, AnyType)
      ((C, Cat): ProductType) fitsNotInto (A, B)
    }
    { // Example 3 from the spec's type allocation examples.
      val A = new TypeVariable("A", Cat, Animal)
      val B = new TypeVariable("B", NothingType, A)
      val C = new TypeVariable("C", Cat, Mammal)
      val D = new TypeVariable("D", ScottishFold, C)
      ((C, D): ProductType) fitsInto (A, B)
    }
    { val A = new TypeVariable("A", NothingType, AnyType)
      val B = new TypeVariable("B", A, AnyType)
      ((ListType(BasicType.String | BasicType.Int), BasicType.Int): ProductType) fitsInto (ListType(B), A)
    }
  }
}
