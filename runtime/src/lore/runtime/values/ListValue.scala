package lore.runtime.values

import lore.types.ListType

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

// TODO: This should extend ObjectValue.
// TODO: Using arrays here sucks if we want an immutable list, like the Scala list. We should rather implement
//       our own singly-linked list or just use the Scala implementation.
@JSExportAll
case class ListValue[A](array: js.Array[A], loreType: ListType) {
  def forEach[R](f: js.Function1[A, R]): Unit = for (e <- array) { f(e) }
  def push(a: A): Unit = array.push(a)
  override def toString: String = s"[$array]"
}
