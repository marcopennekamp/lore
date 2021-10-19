package lore.compiler.syntax.visitor

import lore.compiler.syntax._
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

      case DeclNode.TypeVariableNode(_, lowerBound, upperBound, _, _, _) =>
        val result = concat(visit(lowerBound), visit(upperBound))
        visitor.visit(node, result)

      case DeclNode.AliasNode(_, typeVariables, tpe, _, _) =>
        val result = concat(visit(typeVariables), visit(tpe))
        visitor.visit(node, result)

      case DeclNode.StructNode(_, _, typeVariables, extended, properties, _) =>
        val result = concat(visit(typeVariables), visit(extended), visit(properties))
        visitor.visit(node, result)

      case DeclNode.PropertyNode(_, tpe, _, _, defaultValue, _) =>
        val result = concat(visit(tpe), visit(defaultValue))
        visitor.visit(node, result)

      case DeclNode.TraitNode(_, typeVariables, extended, _) =>
        val result = concat(visit(typeVariables), visit(extended))
        visitor.visit(node, result)

      case node: TypeExprNode => visitTypeExprNode(node)
      case TypeExprNode.ShapePropertyNode(_, tpe, _) => visitor.visit(node, concat(visit(tpe)))

      case exprNode: TopLevelExprNode => visitExprNode(exprNode)
      case ExprNode.AnonymousFunctionParameterNode(_, tpe, _) => visitor.visit(node, concat(visit(tpe)))
      case ExprNode.KeyValueNode(key, value, _) => visitor.visit(node, concat(visit(key), visit(value)))
      case ExprNode.ObjectEntryNode(_, expression, _) => visitor.visit(node, concat(visit(expression)))
      case ExprNode.ShapeValuePropertyNode(_, expression, _) => visitor.visit(node, concat(visit(expression)))
      case ExprNode.CondCaseNode(condition, body, _) => visitor.visit(node, concat(visit(condition), visit(body)))
      case ExprNode.ExtractorNode(_, collection, _) => visitor.visit(node, concat(visit(collection)))
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

    /**
      * We can't use a TopLevelExprVisitor, because it doesn't visit certain nodes such as TypeExprNodes.
      */
    private def visitExprNode(node: TopLevelExprNode): A = node match {
      // These ExprNodes contain nodes that are not TopLevelExprNodes, so we have to handle them specially.
      case TopLevelExprNode.VariableDeclarationNode(_, _, tpe, value, _) => visitor.visit(node, concat(visit(tpe), visit(value)))
      case ExprNode.AnonymousFunctionNode(parameters, body, _) => visitor.visit(node, concat(visit(parameters), visit(body)))
      case ExprNode.FixedFunctionNode(_, argumentTypes, _) => visitor.visit(node, concat(visit(argumentTypes)))
      case ExprNode.ConstructorNode(_, typeArguments, _) => visitor.visit(node, concat(visit(typeArguments)))
      case ExprNode.MapNode(kvs, _) => visitor.visit(node, concat(visit(kvs)))
      case ExprNode.ObjectMapNode(_, typeArguments, entries, _) => visitor.visit(node, concat(visit(typeArguments.toVector.flatten), visit(entries)))
      case ExprNode.ShapeValueNode(properties, _) => visitor.visit(node, concat(visit(properties)))
      case ExprNode.CallNode(target, arguments, _) => visitor.visit(node, concat(visit(target), visit(arguments)))
      case ExprNode.DynamicCallNode(nameLiteral, resultType, arguments, _) => visitor.visit(node, concat(visit(nameLiteral), visit(resultType), visit(arguments)))
      case ExprNode.CondNode(cases, _) => visitor.visit(node, concat(visit(cases)))
      case ExprNode.ForNode(extractors, body, _) => visitor.visit(node, concat(visit(extractors), visit(body)))
      case ExprNode.AscriptionNode(value, expectedType, _) => visitor.visit(node, concat(visit(value), visit(expectedType)))

      // These cases have to be specified last so that we can override these defaults for specific nodes above.
      case node: TopLevelExprNode.LeafNode => visitor.visit(node, concat())
      case node: TopLevelExprNode.UnaryNode => visitor.visit(node, concat(visit(node.child)))
      case node: TopLevelExprNode.BinaryNode => visitor.visit(node, concat(visit(node.child1), visit(node.child2)))
      case node: TopLevelExprNode.TernaryNode => visitor.visit(node, concat(visit(node.child1), visit(node.child2), visit(node.child3)))
      case node: TopLevelExprNode.XaryNode => visitor.visit(node, asResult(node.children.map(visit(_))))
    }

  }

}
