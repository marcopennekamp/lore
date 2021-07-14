package lore.compiler.syntax.visitor

import lore.compiler.syntax.{DeclNode, ExprNode, Node, TopLevelExprNode, TypeDeclNode, TypeExprNode}
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalaz.Id.Id

/**
  * This visitor produces values of type `A` for each node, optionally taking a result of type `R[A]` into account.
  * Using the appropriate [[CombiningNodeVisitor.Applicator]], values of type `A` are computed lazily with `combine` so
  * that the applicator can decide to end the computation early, for example when only a single value is looked for.
  *
  * The result value `R[A]` is created as a concatenation of all values of type `A` produced from the sub-nodes of a
  * given node. For example, the result value for a FunctionNode will be built using `asResult` on the individual `A`
  * values of parameters, the output type, type variables, and the body. Results are be ordered in their lexical
  * occurrence to support node search via this visitor. This operation is also performed lazily to allow the applicator
  * to end the computation early.
  */
trait CombiningNodeVisitor[A, R[_]] {
  protected def visit(node: Node, result: R[A]): A
}

object CombiningNodeVisitor {

  case class VectorApplicator[A](override val visitor: CombiningNodeVisitor[Vector[A], Id]) extends Applicator[Vector[A], Id] {
    override def combine[T](ts: Vector[T], f: T => Vector[A]): Vector[A] = ts.flatMap(f)
    override def asResult(as: Vector[() => Vector[A]]): Vector[A] = as.flatMap(f => f())
  }

  case class OptionApplicator[A](override val visitor: CombiningNodeVisitor[Option[A], Id]) extends Applicator[Option[A], Id] {
    override def combine[T](ts: Vector[T], f: T => Option[A]): Option[A] = ts.firstDefined(f)
    override def asResult(as: Vector[() => Option[A]]): Option[A] = as.firstDefined(f => f())
  }

  trait Applicator[A, R[_]] {

    def visitor: CombiningNodeVisitor[A, R]

    /**
      * Combines the given `ts` <i>lazily</i> using the function `f` into a value of type `A`.
      */
    protected def combine[T](ts: Vector[T], f: T => A): A

    /**
      * Concatenates the given `as` <i>lazily</i> into a result value of type `R[A]`.
      */
    protected def asResult(as: Vector[() => A]): R[A]

    private def visit(nodes: Vector[Node]): A = combine[Node](nodes, visit)
    private def visit(nodeOption: Option[Node]): A = visit(nodeOption.toVector)
    private def concat(as: (() => A)*): R[A] = asResult(as.toVector)

    private implicit def wrapLazy(a: A): () => A = () => a

    def visit(node: Node): A = node match {
      case DeclNode.FunctionNode(_, parameters, outputType, typeVariables, body, _) =>
        val result = concat(visit(parameters), visit(outputType), visit(typeVariables), visit(body))
        visitor.visit(node, result)

      case DeclNode.ParameterNode(_, tpe, _) =>
        visitor.visit(node, concat(visit(tpe)))

      case DeclNode.TypeVariableNode(_, lowerBound, upperBound, _) =>
        val result = concat(visit(lowerBound), visit(upperBound))
        visitor.visit(node, result)

      case TypeDeclNode.AliasNode(_, tpe, _) =>
        visitor.visit(node, concat(visit(tpe)))

      case TypeDeclNode.StructNode(_, extended, properties, _) =>
        val result = concat(visit(extended), visit(properties))
        visitor.visit(node, result)

      case TypeDeclNode.PropertyNode(_, tpe, _, _, defaultValue, _) =>
        val result = concat(visit(tpe), visit(defaultValue))
        visitor.visit(node, result)

      case TypeDeclNode.TraitNode(_, extended, _) =>
        visitor.visit(node, concat(visit(extended)))

      case node: TypeExprNode => visitTypeExprNode(node)
      case TypeExprNode.ShapePropertyNode(_, tpe, _) => visitor.visit(node, concat(visit(tpe)))

      case node: ExprNode => visitExprNode(node)
    }

    private def visitTypeExprNode(node: TypeExprNode): A = node match {
      case TypeExprNode.SumNode(types, _) => visitor.visit(node, concat(visit(types)))
      case TypeExprNode.IntersectionNode(types, _) => visitor.visit(node, concat(visit(types)))
      case TypeExprNode.TupleNode(types, _) => visitor.visit(node, concat(visit(types)))
      case TypeExprNode.FunctionNode(input, output, _) => visitor.visit(node, concat(visit(input), visit(output)))
      case TypeExprNode.ListNode(element, _) => visitor.visit(node, concat(visit(element)))
      case TypeExprNode.MapNode(key, value, _) => visitor.visit(node, concat(visit(key), visit(value)))
      case TypeExprNode.ShapeNode(properties, _) => visitor.visit(node, concat(visit(properties)))
      case _ => visitor.visit(node, concat())
    }

    private def visitExprNode(node: ExprNode): A = TopLevelExprVisitor.visit(ExprVisitor)(node)

    private object ExprVisitor extends CombiningTopLevelExprVisitor.Identity[A] {
      override protected def combine(list: Vector[A]): A = Applicator.this.combine(list, (a: A) => a)

      override protected def visit(node: TopLevelExprNode, results: Vector[A]): A = {
        visitor.visit(node, asResult(results.map(wrapLazy)))
      }
    }

  }

}
