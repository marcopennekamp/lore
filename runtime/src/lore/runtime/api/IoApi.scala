package lore.runtime.api

import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
class IoApi {
  def println(value: Any): Unit = scala.Predef.println(value)
}
