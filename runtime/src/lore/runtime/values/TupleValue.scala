package lore.runtime.values

import lore.types.ProductType

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

/**
  * A Lore tuple value. Tuples are internally represented by arrays as well, for now.
  */
@JSExportAll
case class TupleValue(array: js.Array[Any], loreType: ProductType) {
  override def toString: String = s"($array)"
}
