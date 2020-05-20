package lore.runtime.api

import lore.runtime.types.DeclaredType
import lore.runtime.values.{ListValue, ObjectValue}
import lore.types.ListType

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
class Values {
  def list[A](array: js.Array[A], listType: ListType): ListValue[A] = ListValue(array, listType)
  def `object`(obj: js.Object, tpe: DeclaredType): ObjectValue = ObjectValue(obj, tpe)
}
