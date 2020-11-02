package lore.compiler.types

import org.scalatest.Assertion

class SubtypingSpec extends TypeSpec {
  import TypesExample._

  private implicit class TypeExtension(t1: Type) {
    def <:<(t2: Type): Assertion = assert(t1 <= t2)
    def </<(t2: Type): Assertion = assert(!(t1 <= t2))
  }

  "Subtyping" should "handle sum types correctly" in {
    ((BasicType.String | BasicType.Int) | BasicType.Boolean) <:< (BasicType.String | BasicType.Int | BasicType.Boolean)
    (((BasicType.String | BasicType.Int) | BasicType.Boolean) | BasicType.Int) <:< (BasicType.String | BasicType.Int | BasicType.Boolean)
  }

  it should "handle type variables correctly" in {
    { // An excerpt of Example 1 from the spec's type allocation examples.
      val C = new TypeVariable("C", BasicType.Nothing, BasicType.Any, 0)
      BasicType.Int </< C
    }
    { // An excerpt of Example 2 from the spec's type allocation examples.
      val C = new TypeVariable("C", BasicType.Nothing, BasicType.Any, 0)
      Cat </< C
      C </< Animal
    }
    { // An excerpt of Example 3 from the spec's type allocation examples.
      val C = new TypeVariable("C", Goldfish, Fish, 0)
      val D = new TypeVariable("D", Goldfish, C, 1)
      Goldfish <:< C
      C <:< Animal
      BasicType.Nothing <:< D
      D <:< C
    }
  }
}
