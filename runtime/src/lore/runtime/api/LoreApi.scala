package lore.runtime.api

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("Lore")
@JSExportAll
object LoreApi {
  val types: Types = new Types
  val values: Values = new Values
}
