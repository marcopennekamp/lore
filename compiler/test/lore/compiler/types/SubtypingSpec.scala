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

  it should "handle shape types correctly" in {
    import ShapeTypes._

    Empty <:< Empty

    Position3D <:< Position2D
    Position2D </< Position3D
    Empty </< Position2D
    Empty </< Position3D
    Position2D <:< Empty
    Position3D <:< Empty

    Box2D <:< Position2D
    Box2D </< Position3D
    Position2D </< Box2D
    Position3D </< Box2D
    Empty </< Box2D
    Box2D <:< Empty

    Named </< Sized
    Sized </< Named
    Goldfish <:< Named
    Named </< Goldfish
    Goldfish <:< Sized
    Sized </< Goldfish
    Goldfish <:< NamedAndSized
    NamedAndSized </< Goldfish
    Goldfish <:< Empty
    Empty </< Goldfish
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
