package lore.compiler.types.test

import lore.compiler.types.CompilerSubtyping
import lore.types.{AnyType, BasicType, ListType, NothingType, ProductType, Type, TypeVariable}
import org.scalatest.Assertion

class SubtypingSpec extends TypeSpec {
  import TypesExample._

  private implicit class TypeExtension(t1: Type) {
    def <:<(t2: Type): Assertion = assert(CompilerSubtyping.isSubtype(t1, t2))
    def </<(t2: Type): Assertion = assert(!CompilerSubtyping.isSubtype(t1, t2))
  }

  "Subtyping.isSubtype" should "handle type variables correctly" in {
    { val A = new TypeVariable("A", NothingType, AnyType)
      // ([String], String) <: ([A], A) where A <: Any
      ((ListType(BasicType.String), BasicType.String): ProductType) <:< (ListType(A), A)
      // ([Real], String) </: ([A], A) where A <: Any
      ((ListType(BasicType.Real), BasicType.String): ProductType) </< (ListType(A), A)
      // ([Real], Int) </: ([A], A) where A <: Any
      ((ListType(BasicType.Real), BasicType.Int): ProductType) </< (ListType(A), A)
    }
    { val X = new TypeVariable("X", NothingType, AnyType)
      val Y = new TypeVariable("Y", NothingType, BasicType.Real)
      // (Real, Y) <: (X, Any) where X <: Any, Y <: Real
      ((BasicType.Real, Y): ProductType) <:< (X, AnyType)
    }
    { val A = new TypeVariable("A", NothingType, AnyType)
      val B = new TypeVariable("B", NothingType, A)
      // ([Real], Int) <: ([A], B) where A <: Any, B <: A
      ((ListType(BasicType.Real), BasicType.Int): ProductType) <:< (ListType(A), B)
      // ([T1], T2) <: ([A], B <: A) where A <: Any, B <: A
      ((ListType(Bird), Mammal): ProductType) </< (ListType(A), B)
    }
  }
}
