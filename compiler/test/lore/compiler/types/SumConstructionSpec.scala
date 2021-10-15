package lore.compiler.types

import lore.compiler.utils.CollectionExtensions.VectorExtension
import org.scalatest.Assertion

class SumConstructionSpec extends TypeSpec {

  import TypesExample._

  private implicit class SyntaxExtension(testCase: Product) {
    private val types = testCase.productIterator.toVector.filterType[Type]

    def -->(expected: Type): Assertion = {
      SumType.construct(types) shouldEqual expected
    }
  }

  "SumType.construct" should "combine trait and struct type arguments" in {
    (Cage(Goldfish), Cage(Unicorn), Cage(Mammal)) --> Cage(Goldfish | Mammal)
    (Cage(Unicorn), Cage(ScottishFold)) --> Cage(Unicorn | ScottishFold)
    (Aquarium(Goldfish), Aquarium(Fish)) --> Aquarium(Fish)
    (Aquarium(Goldfish), Cage(Fish)) --> Cage(Fish)
    (Cage(Unicorn), UnicornPen) --> Cage(Unicorn)
    (Function(Fish, Fish), Function(Mammal, Cat)) --> Function(Fish & Mammal, Fish | Cat)
  }

  it should "combine tuple types" in {
    ((Bird, Goldfish): Type, (Raven, Chicken): Type) --> (Bird, Goldfish | Chicken)
    ((Penguin, Chicken, Raven): Type, (Chicken, Raven, Penguin): Type) --> (Penguin | Chicken, Chicken | Raven, Raven | Penguin)
    ((Chicken & Penguin, Goldfish & Cat, Unicorn): Type, (Bird, Fish, Human): Type) --> ((Bird, Fish, Unicorn | Human))
  }

  it should "combine list types" in {
    (ListType(Bird), ListType(Chicken), ListType(Goldfish)) --> ListType(Bird | Goldfish)
    (ListType(Bird & Fish), ListType(Mammal & Fish), ListType(Fish)) --> ListType(Fish)
  }

  /*
  it should "combine map types" in {
    (MapType(BasicType.String, Human), MapType(BasicType.Int, Mammal)) --> MapType(BasicType.String | BasicType.Int, Mammal)
    (MapType(BasicType.Real, Cat), MapType(BasicType.Int, ScottishFold)) --> MapType(BasicType.Int, Cat)
  }
  */

}
