package lore.types

import lore.execution.Context

trait TypeSyntax {
  implicit def toType(name: String)(implicit context: Context): Type = context.types(name)
  implicit def toTupleTypeSS(tuple: (String, String))(implicit context: Context): TupleType = toTupleTypeTT((toType(tuple._1), toType(tuple._2)))
  implicit def toTupleTypeTS(tuple: (Type, String))(implicit context: Context): TupleType = toTupleTypeTT((tuple._1, toType(tuple._2)))
  implicit def toTupleTypeST(tuple: (String, Type))(implicit context: Context): TupleType = toTupleTypeTT((toType(tuple._1), tuple._2))
  implicit def toTupleTypeTT(tuple: (Type, Type))(implicit context: Context): TupleType = TupleType(List(tuple._1, tuple._2))
  implicit class TypeOperators(t1: Type) {
    def &(t2: Type)(implicit context: Context): Type = IntersectionType.construct(Set(t1, t2))
    def |(t2: Type)(implicit context: Context): Type = SumType.construct(Set(t1, t2))
  }
  implicit class StringTypeOperators(s1: String) {
    def &(t2: Type)(implicit context: Context): Type = toType(s1) & t2
    def |(t2: Type)(implicit context: Context): Type = toType(s1) | t2
  }
}
