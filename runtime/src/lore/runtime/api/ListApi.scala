package lore.runtime.api

import lore.runtime.values.ListValue

import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
class ListApi {
  def append[A](list: ListValue[A], element: A): ListValue[A] = {
    // TODO: Don't operate on the list itself but return a new list.
    list.push(element)
    list
  }
}
