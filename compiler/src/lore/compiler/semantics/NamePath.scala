package lore.compiler.semantics

import lore.compiler.syntax.Node.NamePathNode

case class NamePath(segments: Vector[String]) {
  val simpleName: String = segments.lastOption.getOrElse("")
  val headName: String = segments.headOption.getOrElse("")

  val length: Int = segments.length
  lazy val tail: NamePath = NamePath(segments.tail)

  def ++(other: NamePath): NamePath = NamePath(segments ++ other.segments)
  def +(name: String): NamePath = NamePath(segments :+ name)

  /**
    * Returns a new NamePath with the last segment removed, essentially creating the parent of this name path. If this
    * name path is already the empty path (i.e. the root), this function returns None.
    */
  lazy val parent: Option[NamePath] = if (segments.nonEmpty) Some(NamePath(segments.init)) else None
  lazy val parentOrEmpty: NamePath = parent.getOrElse(NamePath.empty)

  val isEmpty: Boolean = segments.isEmpty
  val isRoot: Boolean = isEmpty
  val isSingle: Boolean = segments.length == 1
  val isMultiple: Boolean = segments.length > 1

  override lazy val toString: String = segments.mkString(".")
}

object NamePath {
  def apply(names: String*): NamePath = NamePath(names.toVector)
  def apply(namePathNode: NamePathNode): NamePath = NamePath(namePathNode.segments.map(_.value))

  val empty: NamePath = NamePath(Vector.empty)
}
