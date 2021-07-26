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
    // TODO (schemas): Write test.
  }

}
