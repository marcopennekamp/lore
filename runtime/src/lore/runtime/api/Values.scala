package lore.runtime.api

import lore.runtime.types.DeclaredType
import lore.runtime.values.{ListValue, ObjectValue, TupleValue}
import lore.types.{ListType, ProductType}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
class Values {
  def list[A](array: js.Array[A], tpe: ListType): ListValue[A] = ListValue(array, tpe)
  def tuple(array: js.Array[Any], tpe: ProductType): TupleValue = TupleValue(array, tpe)
  val unit: TupleValue = TupleValue(js.Array(), ProductType.UnitType)
  def `object`(obj: js.Object, tpe: DeclaredType): ObjectValue = ObjectValue(obj, tpe)
}
