package lore.compiler.types

import lore.compiler.test.BaseSpec
import org.scalatest.Assertion

class TypeEncoderSpec extends BaseSpec {

  implicit class EncodingExtension(tpe: Type) {
    def -->(expected: Vector[Int]): Assertion = {
      TypeEncoder.encode(tpe) shouldEqual expected.map(_.toByte)
    }
  }

  "TypeEncoder" should "correctly encode basic types" in {
    BasicType.Any --> Vector(0xa0)
    BasicType.Nothing --> Vector(0xa1)
    BasicType.Real --> Vector(0xa2)
    BasicType.Int --> Vector(0xa3)
    BasicType.Boolean --> Vector(0xa4)
    BasicType.String --> Vector(0xa5)
  }

  it should "correctly encode list and map types with basic type children" in {
    ListType(BasicType.Real) --> Vector(0xc1, 0xa2)
    ListType(BasicType.String) --> Vector(0xc1, 0xa5)
    MapType(BasicType.String, BasicType.Any) --> Vector(0xc2, 0xa5, 0xa0)
    MapType(BasicType.String, BasicType.Boolean) --> Vector(0xc2, 0xa5, 0xa4)
  }

  it should "correctly encode type variables with basic type bounds" in {
    new TypeVariable("A", BasicType.Nothing, BasicType.Any, 0) --> Vector(0xc4, 0x01, 'A')
    new TypeVariable("Num", BasicType.Int, BasicType.Any, 0) --> Vector(0xc5, 0x03, 'N', 'u', 'm', 0xa3)
    new TypeVariable("π", BasicType.Nothing, BasicType.Real, 0) --> Vector(0xc6, 0x02, 0xCF, 0x80, 0xa2)
    new TypeVariable("C", BasicType.Boolean, BasicType.String, 0) --> Vector(0xc7, 0x01, 'C', 0xa4, 0xa5)
  }

  it should "correctly encode sum, intersection, and product types with basic type children" in {
    // TODO: Write this test.
  }

  it should "correctly encode named types" in {
    // TODO: Write this test.
  }

  it should "correctly encode shape types" in {
    // TODO: Write this test.
  }

  it should "correctly encode nested type constructors with basic type leaves" in {
    // TODO: Write this test.
  }

  it should "correctly encode complex types with named type leaves" in {
    // TODO: Write this test.
  }

}
