package lore.types

case class ProductType(components: List[Type]) extends Type {
  /**
    * Since this is already a tuple, there is no need to enclose it in another tuple.
    */
  override def toTuple: ProductType = this

  /**
    * Whether the product type is abstract. A product type is abstract if one of its component types is abstract.
    */
  override def isAbstract: Boolean = components.exists(_.isAbstract)

  override def toString: String = s"(${components.mkString(", ")})"
}

object ProductType {
  val UnitType: ProductType = ProductType(List.empty)
}
