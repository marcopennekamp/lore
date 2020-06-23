package lore.compiler.types.test

import lore.types._
import org.scalatest.Assertion

class SubtypingSpec extends TypeSpec {
  import TypesExample._

  private implicit class TypeExtension(t1: Type) {
    def <:<(t2: Type): Assertion = assert(t1 <= t2)
    def </<(t2: Type): Assertion = assert(!(t1 <= t2))
  }

  "Subtyping" should "handle type variables correctly" in {
    { // An excerpt of Example 1 from the spec's type allocation examples.
      val C = new TypeVariable("C", NothingType, AnyType)
      BasicType.Int </< C
    }
    { // An excerpt of Example 2 from the spec's type allocation examples.
      val C = new TypeVariable("C", NothingType, AnyType)
      Cat </< C
      C </< Animal
    }
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
