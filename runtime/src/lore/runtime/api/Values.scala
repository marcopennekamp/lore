package lore.runtime.api

import lore.runtime.values
import lore.runtime.values.ListValue
import lore.types.ListType

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
class Values {
  def list[A](array: js.Array[A], listType: ListType): ListValue[A] = values.ListValue(array, listType)
}
