package lore.types

import lore.execution.Context

trait TypeSyntax {
  implicit def toType(name: String)(implicit context: Context): Type = context.types(name)
  implicit def toProductTypeSS(tuple: (String, String))(implicit context: Context): ProductType = toProductTypeTT((toType(tuple._1), toType(tuple._2)))
  implicit def toProductTypeTS(tuple: (Type, String))(implicit context: Context): ProductType = toProductTypeTT((tuple._1, toType(tuple._2)))
  implicit def toProductTypeST(tuple: (String, Type))(implicit context: Context): ProductType = toProductTypeTT((toType(tuple._1), tuple._2))
  implicit def toProductTypeTT(tuple: (Type, Type))(implicit context: Context): ProductType = ProductType(List(tuple._1, tuple._2))
  implicit class TypeOperators(t1: Type) {
    def &(t2: Type)(implicit context: Context): Type = IntersectionType.construct(Set(t1, t2))
    def |(t2: Type)(implicit context: Context): Type = SumType.construct(Set(t1, t2))
  }
  implicit class StringTypeOperators(s1: String) {
    def &(t2: Type)(implicit context: Context): Type = toType(s1) & t2
    def |(t2: Type)(implicit context: Context): Type = toType(s1) | t2
  }
}
