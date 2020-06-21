package lore.compiler.types.test

import lore.compiler.types.CompilerSubtyping
import lore.types.{AnyType, Assignability, BasicType, ListType, NothingType, ProductType, Type, TypeVariable}
import org.scalatest.Assertion

class SubtypingSpec extends TypeSpec {
  import TypesExample._

  private implicit class TypeExtension(t1: Type) {
    def <:<(t2: Type): Assertion = assert(t1 <= t2)
    def </<(t2: Type): Assertion = assert(!(t1 <= t2))
    def assignableTo(t2: Type): Assertion = {
      println(s"$t1 assignableTo $t2?")
      assert(Assignability.isAssignable(t1, t2))
    }
    def notAssignableTo(t2: Type): Assertion = assert(!Assignability.isAssignable(t1, t2))
  }

  "Assignability.isAssignable" should "handle type variables correctly" in {
    { val A = new TypeVariable("A", NothingType, AnyType)
      // ([String], String) is assignable to ([A], A) where A <: Any
      ((ListType(BasicType.String), BasicType.String): ProductType) assignableTo (ListType(A), A)
      // ([Real], String) is NOT assignable to ([A], A) where A <: Any
      ((ListType(BasicType.Real), BasicType.String): ProductType) notAssignableTo (ListType(A), A)
      // ([Real], Int) is NOT assignable to ([A], A) where A <: Any
      ((ListType(BasicType.Real), BasicType.Int): ProductType) notAssignableTo (ListType(A), A)
    }
    { val X = new TypeVariable("X", NothingType, AnyType)
      val Y = new TypeVariable("Y", NothingType, BasicType.Real)
      // (Real, Y) is assignable to (X, Any) where X <: Any, Y <: Real
      ((BasicType.Real, Y): ProductType) assignableTo (X, AnyType)
    }
    { val A = new TypeVariable("A", NothingType, AnyType)
      val B = new TypeVariable("B", NothingType, A)
      // ([Real], Int) is assignable to ([A], B) where A <: Any, B <: A
      ((ListType(BasicType.Real), BasicType.Int): ProductType) assignableTo (ListType(A), B)
      // ([T1], T2) is NOT assignable to ([A], B <: A) where A <: Any, B <: A
      ((ListType(Bird), Mammal): ProductType) notAssignableTo (ListType(A), B)
    }
    { // Example 3 from the spec's type allocation examples.
      val A = new TypeVariable("A", Cat, Animal)
      val B = new TypeVariable("B", NothingType, A)
      val C = new TypeVariable("C", Cat, Mammal)
      val D = new TypeVariable("D", ScottishFold, C)
      ((C, D): ProductType) assignableTo (A, B)
    }
  }

  "Subtyping.isSubtype" should "handle type variables correctly" in {
    { // An excerpt of Example 3 from the spec's type allocation examples.
      val C = new TypeVariable("C", Goldfish, Fish)
      val D = new TypeVariable("D", Goldfish, C)
      Goldfish <:< C
      C <:< Animal
      NothingType <:< D
      D <:< C
    }
  }
}
