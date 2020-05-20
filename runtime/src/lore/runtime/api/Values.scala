package lore.runtime.api

import lore.types.{ListType, Type}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}

@JSExportTopLevel("LoreList")
@JSExportAll
case class LoreList[A](array: js.Array[A], loreType: ListType) {
  def forEach[R](f: js.Function1[A, R]): Unit = {
    println("foreach")
    for (e <- array) { f(e) }
  }
  def push(a: A): Unit = {
    println(a)
    array.push(a)
  }
}

@JSExportTopLevel("Values")
object Values {
  @JSExport
  def list[A](array: js.Array[A], elementType: Type): LoreList[A] = LoreList(array, ListType(elementType))
}
