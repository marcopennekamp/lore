package lore.compiler.test

import lore.compiler.semantics.Registry
import lore.compiler.types._

trait TypeSyntax {
  implicit def toType(name: String)(implicit registry: Registry): Type = registry.getType(name).get
  implicit def toProductTypeSS(tuple: (String, String))(implicit registry: Registry): ProductType = toProductTypeTT((toType(tuple._1), toType(tuple._2)))
  implicit def toProductTypeTS(tuple: (Type, String))(implicit registry: Registry): ProductType = toProductTypeTT((tuple._1, toType(tuple._2)))
  implicit def toProductTypeST(tuple: (String, Type))(implicit registry: Registry): ProductType = toProductTypeTT((toType(tuple._1), tuple._2))
  implicit def toProductTypeTT(tuple: (Type, Type)): ProductType = ProductType(Vector(tuple._1, tuple._2))
  implicit def toProductTypeTTT(tuple: (Type, Type, Type)): ProductType = ProductType(Vector(tuple._1, tuple._2, tuple._3))
  implicit class TypeOperators(t1: Type) {
    def &(t2: Type): Type = IntersectionType.construct(Vector(t1, t2))
    def |(t2: Type): Type = SumType.construct(Vector(t1, t2))
  }
  implicit class StringTypeOperators(s1: String) {
    def &(t2: Type)(implicit registry: Registry): Type = toType(s1) & t2
    def |(t2: Type)(implicit registry: Registry): Type = toType(s1) | t2
  }
}
