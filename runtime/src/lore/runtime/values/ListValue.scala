package lore.runtime.values

import lore.types.ListType

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
case class ListValue[A](array: js.Array[A], loreType: ListType) {
  def forEach[R](f: js.Function1[A, R]): Unit = for (e <- array) { f(e) }
  def push(a: A): Unit = array.push(a)
}
