package lore.compiler.semantics

import lore.compiler.syntax.Node.NamePathNode

case class NamePath(segments: Vector[NamePath.Segment]) {
  val simpleName: String = segments.lastOption.map(_.name).getOrElse("")

  val length: Int = segments.length
  lazy val tail: NamePath = NamePath(segments.tail)

  def concat(other: NamePath): NamePath = NamePath(segments ++ other.segments)
  def ++(other: NamePath): NamePath = concat(other)

  def append(other: String): NamePath = NamePath(segments :+ NamePath.Segment(other))
  def +(name: String): NamePath = append(name)

  /**
    * Returns a new NamePath with the last segment removed, essentially creating the parent of this name path. If this
    * name path is already the empty path (i.e. the root), this function returns None.
    */
  lazy val parent: Option[NamePath] = if (segments.nonEmpty) Some(NamePath(segments.init)) else None
  lazy val parentOrEmpty: NamePath = parent.getOrElse(NamePath.empty)

  val isEmpty: Boolean = segments.isEmpty

  override lazy val toString: String = segments.mkString(".")
}

object NamePath {
  case class Segment(name: String) {
    override val toString: String = name
  }

  def from(namePathNode: NamePathNode): NamePath = {
    NamePath(namePathNode.segments.map(node => NamePath.Segment(node.value)))
  }

  val empty: NamePath = NamePath(Vector.empty)
}
