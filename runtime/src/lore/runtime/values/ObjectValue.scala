package lore.runtime.values

import lore.runtime.types.DeclaredType

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
case class ObjectValue(obj: js.Object, loreType: DeclaredType)
