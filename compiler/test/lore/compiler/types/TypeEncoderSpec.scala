package lore.compiler.types

import lore.compiler.test.{BaseSpec, TypeSyntax}
import org.scalatest.Assertion

class TypeEncoderSpec extends BaseSpec with TypeSyntax {

  implicit class EncodingExtension(tpe: Type) {
    def -->(expected: Vector[Int]): Assertion = {
      TypeEncoder.encode(tpe) shouldEqual expected.map(_.toByte)
    }
  }

  "TypeEncoder" should "correctly encode basic types" in {
    BasicType.Any --> Vector(0xa0)
    BasicType.Nothing --> Vector(0xa1)
    BasicType.Number --> Vector(0xa2)
    BasicType.Boolean --> Vector(0xa3)
    BasicType.String --> Vector(0xa4)
  }

  it should "correctly encode list and map types with basic type children" in {
    ListType(BasicType.Number) --> Vector(0xc1, 0xa2)
    ListType(BasicType.String) --> Vector(0xc1, 0xa4)
    MapType(BasicType.String, BasicType.Any) --> Vector(0xc2, 0xa4, 0xa0)
    MapType(BasicType.String, BasicType.Boolean) --> Vector(0xc2, 0xa4, 0xa3)
  }

  it should "correctly encode type variables with basic type bounds" in {
    typeVariable("A") --> Vector(0xc4, 0x01, 'A')
    typeVariable("Num", lowerBound = BasicType.Number) --> Vector(0xc5, 0x03, 'N', 'u', 'm', 0xa2)
    typeVariable("π", upperBound = BasicType.Number) --> Vector(0xc6, 0x02, 0xCF, 0x80, 0xa2)
    typeVariable("C", BasicType.Boolean, BasicType.String) --> Vector(0xc7, 0x01, 'C', 0xa3, 0xa4)
  }

}
