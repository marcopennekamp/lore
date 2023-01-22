package lore.compiler.syntax

import lore.compiler.core.Position
import lore.compiler.syntax.DeclNode.TypeVariableNode
import lore.compiler.syntax.Node.NameNode

trait AnnotationNode extends Node

/**
  * An annotation which consists only of a name, such as `@root`.
  */
case class SimpleAnnotationNode(name: NameNode) extends AnnotationNode {
  override def position: Position = name.position
}

/**
  * An `@where` annotation with at least one type parameter.
  */
case class WhereAnnotationNode(
  typeParameters: Vector[TypeVariableNode],
  override val position: Position,
) extends AnnotationNode
