package lore.runtime

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js

@JSExportTopLevel("Types")
object Types {
  @JSExport
  def test(): Unit = println("Hello Types")

  trait Type

  @JSExport
  def typeof(value: Any): Type = {
    js.typeOf(value) match {
      case _ => ???
    }
  }
}
