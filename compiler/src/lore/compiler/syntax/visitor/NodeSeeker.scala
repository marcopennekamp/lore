package lore.compiler.syntax.visitor

import lore.compiler.core.Position
import lore.compiler.syntax._
import lore.compiler.utils.CollectionExtensions.VectorExtension

/**
  * Searches for the first [[Node]] that encompasses the target position and for which the `handle` function returns a
  * `Some`. The first node is defined in a depth-first ordering.
  */
trait NodeSeeker[A] {

  /**
    * Handle the node found at the target position, returning `Some` with a result value if it is an acceptable node.
    */
  protected def result(node: Node): Option[A]

  def seek(node: DeclNode, target: NodeSeeker.Target): Option[A] = {
    node match {
      case DeclNode.FunctionNode(_, parameters, outputType, typeVariables, body, _) =>
        typeVariables.firstDefined(handleTypeVariableNode(_, target))
          .orElse(parameters.firstDefined(parameter => handleTypeExprNode(parameter.tpe, target))
          .orElse(handleTypeExprNode(outputType, target))
          .orElse(body.flatMap(handleExprNode(_, target)))
          .orElse(check(node, target)))

      case TypeDeclNode.AliasNode(_, tpe, _) =>
        handleTypeExprNode(tpe, target)
          .orElse(check(node, target))

      case TypeDeclNode.StructNode(_, extended, properties, _) =>
        extended.firstDefined(handleTypeExprNode(_, target))
          .orElse(properties.firstDefined(handleStructPropertyNode(_, target)))
          .orElse(check(node, target))

      case TypeDeclNode.TraitNode(_, extended, _) =>
        extended.firstDefined(handleTypeExprNode(_, target))
          .orElse(check(node, target))
    }
  }

  private def handleTypeVariableNode(node: DeclNode.TypeVariableNode, target: NodeSeeker.Target): Option[A] = {
    node.lowerBound.flatMap(handleTypeExprNode(_, target))
      .orElse(node.upperBound.flatMap(handleTypeExprNode(_, target)))
      .orElse(check(node, target))
  }

  private def handleStructPropertyNode(node: TypeDeclNode.PropertyNode, target: NodeSeeker.Target): Option[A] = {
    handleTypeExprNode(node.tpe, target)
      .orElse(node.defaultValue.flatMap(handleExprNode(_, target)))
      .orElse(check(node, target))
  }

  private def handleTypeExprNode(node: TypeExprNode, target: NodeSeeker.Target): Option[A] = {
    val rec = handleTypeExprNode(_, target)
    node match {
      case TypeExprNode.SumNode(types, _) => types.firstDefined(rec).orElse(check(node, target))
      case TypeExprNode.IntersectionNode(types, _) => types.firstDefined(rec).orElse(check(node, target))
      case TypeExprNode.TupleNode(types, _) => types.firstDefined(rec).orElse(check(node, target))
      case TypeExprNode.FunctionNode(input, output, _) => rec(input).orElse(rec(output)).orElse(check(node, target))
      case TypeExprNode.ListNode(element, _) => rec(element).orElse(check(node, target))
      case TypeExprNode.MapNode(key, value, _) => rec(key).orElse(rec(value)).orElse(check(node, target))
      case TypeExprNode.ShapeNode(properties, _) =>
        properties.firstDefined(
          property => rec(property.tpe).orElse(check(property, target))
        ).orElse(check(node, target))
      case _ => check(node, target)
    }
  }

  private def handleExprNode(node: ExprNode, target: NodeSeeker.Target): Option[A] = {
    TopLevelExprVisitor.visit(NodeSeeker.ExprVisitor(this, target))(node)
  }

  private def check(node: Node, target: NodeSeeker.Target): Option[A] = {
    if (target.containedIn(node.position)) result(node) else None
  }

}

object NodeSeeker {

  case class Target(line: Int, column: Int) {
    def containedIn(position: Position): Boolean = containedIn(position.startLine, position.startColumn, position.endLine, position.endColumn)

    def containedIn(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Boolean = {
      (startLine < this.line || startLine == this.line && startColumn <= this.column) &&
        (this.line < endLine || endLine == this.line && this.column <= endColumn)
    }
  }

  private case class ExprVisitor[A](seeker: NodeSeeker[A], target: NodeSeeker.Target) extends CombiningTopLevelExprVisitor.Identity[Option[A]] {
    override protected def combine(list: Vector[Option[A]]): Option[A] = list.find(_.isDefined).flatten

    override protected def visit(node: TopLevelExprNode, results: Vector[Option[A]]): Option[A] = {
      combine(results).orElse(seeker.check(node, target))
    }
  }

}
