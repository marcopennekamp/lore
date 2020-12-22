package lore.compiler.types

import lore.compiler.test.BaseSpec
import org.scalatest.Assertion

class TypeEncoderSpec extends BaseSpec {

  implicit class EncodingExtension(tpe: Type) {
    def -->(expected: Array[Int]): Assertion = {
      TypeEncoder.encode(tpe) shouldEqual expected.map(_.toByte)
    }
  }

  "TypeEncoder" should "correctly encode basic types" in {
    BasicType.Any --> Array(0x80)
    BasicType.Nothing --> Array(0x81)
    BasicType.Real --> Array(0x82)
    BasicType.Int --> Array(0x83)
    BasicType.Boolean --> Array(0x84)
    BasicType.String --> Array(0x85)
  }

  it should "correctly encode list and map types with basic type children" in {
    ListType(BasicType.Real) --> Array(0xa0, 0x82)
    ListType(BasicType.String) --> Array(0xa0, 0x85)
    MapType(BasicType.String, BasicType.Any) --> Array(0xa1, 0x85, 0x80)
    MapType(BasicType.String, BasicType.Boolean) --> Array(0xa1, 0x85, 0x84)
  }

  it should "correctly encode type variables with basic type bounds" in {
    new TypeVariable("A", BasicType.Nothing, BasicType.Any, 0) --> Array(0xa4, 0x01, 'A')
    new TypeVariable("Num", BasicType.Int, BasicType.Any, 0) --> Array(0xa5, 0x03, 'N', 'u', 'm', 0x83)
    new TypeVariable("π", BasicType.Nothing, BasicType.Real, 0) --> Array(0xa6, 0x02, 0xCF, 0x80, 0x82)
    new TypeVariable("C", BasicType.Boolean, BasicType.String, 0) --> Array(0xa7, 0x01, 'C', 0x84, 0x85)
  }

  it should "correctly encode sum, intersection, and product types with basic type children" in {
    // TODO: Write this test.
  }

  it should "correctly encode named types" in {
    // TODO: Write this test.
  }

  it should "correctly encode nested type constructors with basic type leaves" in {
    // TODO: Write this test.
  }

  it should "correctly encode complex types with named type leaves" in {
    // TODO: Write this test.
  }

}
