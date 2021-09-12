package lore.compiler.semantics

import lore.compiler.syntax.Node.NamePathNode

case class NamePath(segments: Vector[NamePath.Segment]) {
  val simpleName: String = segments.lastOption.map(_.name).getOrElse("")

  def concat(other: NamePath): NamePath = NamePath(segments ++ other.segments)
  def append(other: String): NamePath = NamePath(segments :+ NamePath.Segment(other))

  /**
    * Creates a new NamePath with the last segment removed, essentially creating the parent of this name path. If this
    * name path is already the empty path (i.e. the root), this function returns None.
    */
  def parent: Option[NamePath] = if (segments.nonEmpty) Some(NamePath(segments.init)) else None
}

object NamePath {
  case class Segment(name: String)

  def from(namePathNode: NamePathNode): NamePath = {
    NamePath(namePathNode.segments.map(node => NamePath.Segment(node.value)))
  }

  val empty: NamePath = NamePath(Vector.empty)
}
