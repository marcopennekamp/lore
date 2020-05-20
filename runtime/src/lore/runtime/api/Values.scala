package lore.runtime.api

import lore.types.{ListType, Type}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Values")
object Values {
  case class LoreList[A](array: js.Array[A], loreType: ListType)

  @JSExport
  def list[A](array: js.Array[A], elementType: Type): LoreList[A] = LoreList(array, ListType(elementType))
}
