package lore.compiler.syntax

import lore.compiler.core.Position
import lore.compiler.syntax.DeclNode.TypeVariableNode
import lore.compiler.syntax.Node.NameNode

trait AnnotationNode extends Node {
  /**
    * The unique name of the annotation (including the `@`). It is used for error reporting and finding duplicate
    * annotations.
    */
  def uniqueName: String

  /**
    * Whether this annotation may annotate the same declaration multiple times.
    */
  def areMultipleAllowed: Boolean = false
}

/**
  * An annotation which consists only of a name, such as `@root`.
  */
case class SimpleAnnotationNode(name: NameNode) extends AnnotationNode {
  override def uniqueName: String = s"@${name.value}"
  override def position: Position = name.position
}

/**
  * An `@where` annotation with at least one type parameter.
  */
case class WhereAnnotationNode(
  typeParameters: Vector[TypeVariableNode],
  override val position: Position,
) extends AnnotationNode {
  override def uniqueName: String = "@where"
}
