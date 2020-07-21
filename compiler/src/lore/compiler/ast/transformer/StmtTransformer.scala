package lore.compiler.ast.transformer

import lore.compiler.ast.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.core.Compilation

// TODO: Test that StmtTransformer now correctly copies over a node's state.

/**
  * Visits any statement node, possibly transforming it to another kind of node. Subtrees are visited automatically.
  * The default implementations re-create parent nodes for only those nodes where at least a child node changed.
  *
  * TODO: Having to specify a method for each and every node sucks. Any better way?
  */
trait StmtTransformer {
  private def default[N <: StmtNode.UnaryNode](node: N, child: StmtNode)(transformed: => N): Compilation[N] = {
    Compilation.succeed(if (node.child == child) node else transformed)
  }
  private def default[N <: StmtNode.BinaryNode](node: N, child1: StmtNode, child2: StmtNode)(transformed: => N): Compilation[N] = {
    Compilation.succeed(if (node.child1 == child1 && node.child2 == child2) node else transformed)
  }
  private def default[N <: StmtNode.TernaryNode](node: N, child1: StmtNode, child2: StmtNode, child3: StmtNode)(transformed: => N): Compilation[N] = {
    Compilation.succeed(if (node.child1 == child1 && node.child2 == child2 && node.child3 == child3) node else transformed)
  }
  private def default[N <: StmtNode.XaryNode](node: N, children: List[StmtNode])(transformed: => N): Compilation[N] = {
    Compilation.succeed(if (node.children == children) node else transformed)
  }

  // Leaf nodes.
  def transform(node: StmtNode.LeafNode): Compilation[ExprNode] = Compilation.succeed(node)

  // Unary nodes.
  def transform(node: StmtNode.ReturnNode)(expr: ExprNode): Compilation[StmtNode] = {
    default(node, expr) { node.copy(expr) }
  }
  def transform(node: TopLevelExprNode.VariableDeclarationNode)(value: ExprNode): Compilation[TopLevelExprNode] = {
    default(node, value) { node.copy(value = value) }
  }
  def transform(node: ExprNode.NegationNode)(expr: ExprNode): Compilation[ExprNode] = {
    default(node, expr) { node.copy(expr) }
  }
  def transform(node: ExprNode.LogicalNotNode)(expr: ExprNode): Compilation[ExprNode] = {
    default(node, expr) { node.copy(expr) }
  }
  def transform(node: ExprNode.PropertyAccessNode)(instance: ExprNode): Compilation[ExprNode] = {
    default(node, instance) { node.copy(instance = instance) }
  }

  // Binary nodes.
  def transform(node: TopLevelExprNode.AssignmentNode)(address: ExprNode.AddressNode, value: ExprNode): Compilation[TopLevelExprNode] = {
    default(node, address, value) { node.copy(address = address, value = value) }
  }
  def transform(node: ExprNode.AdditionNode)(left: ExprNode, right: ExprNode): Compilation[ExprNode] = {
    default(node, left, right) { node.copy(left = left, right = right) }
  }
  def transform(node: ExprNode.SubtractionNode)(left: ExprNode, right: ExprNode): Compilation[ExprNode] = {
    default(node, left, right) { node.copy(left = left, right = right) }
  }
  def transform(node: ExprNode.MultiplicationNode)(left: ExprNode, right: ExprNode): Compilation[ExprNode] = {
    default(node, left, right) { node.copy(left = left, right = right) }
  }
  def transform(node: ExprNode.DivisionNode)(left: ExprNode, right: ExprNode): Compilation[ExprNode] = {
    default(node, left, right) { node.copy(left = left, right = right) }
  }
  def transform(node: ExprNode.EqualsNode)(left: ExprNode, right: ExprNode): Compilation[ExprNode] = {
    default(node, left, right) { node.copy(left = left, right = right) }
  }
  def transform(node: ExprNode.NotEqualsNode)(left: ExprNode, right: ExprNode): Compilation[ExprNode] = {
    default(node, left, right) { node.copy(left = left, right = right) }
  }
  def transform(node: ExprNode.LessThanNode)(left: ExprNode, right: ExprNode): Compilation[ExprNode] = {
    default(node, left, right) { node.copy(left = left, right = right) }
  }
  def transform(node: ExprNode.LessThanEqualsNode)(left: ExprNode, right: ExprNode): Compilation[ExprNode] = {
    default(node, left, right) { node.copy(left = left, right = right) }
  }
  def transform(node: ExprNode.GreaterThanNode)(left: ExprNode, right: ExprNode): Compilation[ExprNode] = {
    default(node, left, right) { node.copy(left = left, right = right) }
  }
  def transform(node: ExprNode.GreaterThanEqualsNode)(left: ExprNode, right: ExprNode): Compilation[ExprNode] = {
    default(node, left, right) { node.copy(left = left, right = right) }
  }
  def transform(node: ExprNode.RepetitionNode)(condition: ExprNode, body: StmtNode): Compilation[ExprNode] = {
    default(node, condition, body) { node.copy(condition = condition, body = body) }
  }

  // Ternary nodes.
  def transform(node: ExprNode.IfElseNode)(condition: ExprNode, onTrue: StmtNode, onFalse: StmtNode): Compilation[ExprNode] = {
    default(node, condition, onTrue, onFalse) { node.copy(condition = condition, onTrue = onTrue, onFalse = onFalse) }
  }

  // Xary nodes.
  def transform(node: TopLevelExprNode.ConstructorCallNode)(arguments: List[ExprNode]): Compilation[TopLevelExprNode] = {
    default(node, arguments) { node.copy(arguments = arguments) }
  }
  def transform(node: TopLevelExprNode.ConstructNode)(arguments: List[ExprNode]): Compilation[TopLevelExprNode] = {
    default(node, arguments) { node.copy(arguments = arguments) }
  }
  def transform(node: ExprNode.ConjunctionNode)(expressions: List[ExprNode]): Compilation[ExprNode] = {
    default(node, expressions) { node.copy(expressions = expressions) }
  }
  def transform(node: ExprNode.DisjunctionNode)(expressions: List[ExprNode]): Compilation[ExprNode] = {
    default(node, expressions) { node.copy(expressions = expressions) }
  }
  def transform(node: ExprNode.ConcatenationNode)(expressions: List[ExprNode]): Compilation[ExprNode] = {
    default(node, expressions) { node.copy(expressions = expressions) }
  }
  def transform(node: ExprNode.TupleNode)(expressions: List[ExprNode]): Compilation[ExprNode] = {
    default(node, expressions) { node.copy(expressions = expressions) }
  }
  def transform(node: ExprNode.ListNode)(expressions: List[ExprNode]): Compilation[ExprNode] = {
    default(node, expressions) { node.copy(expressions = expressions) }
  }
  def transform(node: ExprNode.BlockNode)(statements: List[ExprNode]): Compilation[ExprNode] = {
    default(node, statements) { node.copy(statements = statements) }
  }
  def transform(node: ExprNode.SimpleCallNode)(arguments: List[ExprNode]): Compilation[ExprNode] = {
    default(node, arguments) { node.copy(arguments = arguments) }
  }
  def transform(node: ExprNode.FixedFunctionCallNode)(arguments: List[ExprNode]): Compilation[ExprNode] = {
    default(node, arguments) { node.copy(arguments = arguments) }
  }
  def transform(node: ExprNode.DynamicCallNode)(arguments: List[ExprNode]): Compilation[ExprNode] = {
    default(node, arguments) { node.copy(arguments = arguments) }
  }

  // Map nodes.
  def transform(node: ExprNode.MapNode)(entries: List[ExprNode.KeyValueNode]): Compilation[ExprNode] = {
    Compilation.succeed(if (node.kvs == entries) node else node.copy(kvs = entries))
  }
  def transform(node: ExprNode.KeyValueNode)(key: ExprNode, value: ExprNode): Compilation[ExprNode.KeyValueNode] = {
    Compilation.succeed(if (node.key == key && node.value == value) node else node.copy(key = key, value = value))
  }

  // Iteration nodes.
  def transform(node: ExprNode.IterationNode)(extractors: List[ExprNode.ExtractorNode], body: StmtNode): Compilation[ExprNode] = {
    Compilation.succeed(if (node.extractors == extractors && node.body == body) node else node.copy(extractors = extractors, body = body))
  }
  def transform(node: ExprNode.ExtractorNode)(collection: ExprNode): Compilation[ExprNode.ExtractorNode] = {
    Compilation.succeed(if (node.collection == collection) node else node.copy(collection = collection))
  }
}

object StmtTransformer {
  class Applicator[Props](transformer: StmtTransformer) {
    /**
      * Transforms the whole tree invoking transform* functions for every node.
      */
    final def transform(node: StmtNode, props: Props): Compilation[StmtNode] = {
      handleMatch(node, props)
    }

    protected def handleMatch[A](node: A, props: Props): Compilation[StmtNode] = {
      // The cast is necessary because we can't define a generic handleMatch which would return the right kind of
      // compilation based on the node type that we matched on. It seems this is a limitation of match-based
      // polymorphism.
      def rec[N](node: StmtNode, props: Props): Compilation[N] = handleMatch[StmtNode](node, props).asInstanceOf[Compilation[N]]
      def recBinary[C1, C2](child1: StmtNode, child2: StmtNode, props: Props): Compilation[(C1, C2)] = {
        (rec[C1](child1, props), rec[C2](child2, props)).simultaneous
      }
      def recXary[C](children: List[StmtNode], props: Props): Compilation[List[C]] = {
        children.map(child => rec[C](child, props)).simultaneous
      }

      // Importing all statement nodes makes the code far easier to digest visually.
      import ExprNode._
      import StmtNode._

      node match {
        // Oh, how sweet the warm embrace of multiple dispatch, how cold the torment of the matching soul.
        // Give me your node for I shall decide its faith and yet grant safety to those who come in passing.
        // Give me your recursion and I shall dispatch and yet grant safety to those who come in return.
        // Leave your errors with me, for they shall never plague you with unexpected kinds and types any more.
        // Leave your burdens with me, for you shall never again write as much as this and achieve so little in return.
        // Oh, the sweet, warm embrace of multiple dispatch. Oh, the cold torment of the matching soul.
        // Forgive me, Scala.

        // Leaf nodes.
        case node: LeafNode => transformer.transform(node)

        // Unary nodes.
        case node@ReturnNode(expr, _) => rec[ExprNode](expr, props).flatMap(transformer.transform(node))
        case node@TopLevelExprNode.VariableDeclarationNode(_, _, _, value, _) => rec[ExprNode](value, props).flatMap(transformer.transform(node))
        case node@NegationNode(expr, _) => rec[ExprNode](expr, props).flatMap(transformer.transform(node))
        case node@LogicalNotNode(expr, _) => rec[ExprNode](expr, props).flatMap(transformer.transform(node))
        case node@PropertyAccessNode(instance, _, _) => rec[ExprNode](instance, props).flatMap(transformer.transform(node))

        // Binary nodes.
        case node@TopLevelExprNode.AssignmentNode(address, value, _) =>
          recBinary[AddressNode, ExprNode](address, value, props).flatMap((transformer.transform(node) _).tupled)
        case node@AdditionNode(left, right, _) =>
          recBinary[ExprNode, ExprNode](left, right, props).flatMap((transformer.transform(node) _).tupled)
        case node@SubtractionNode(left, right, _) =>
          recBinary[ExprNode, ExprNode](left, right, props).flatMap((transformer.transform(node) _).tupled)
        case node@MultiplicationNode(left, right, _) =>
          recBinary[ExprNode, ExprNode](left, right, props).flatMap((transformer.transform(node) _).tupled)
        case node@DivisionNode(left, right, _) =>
          recBinary[ExprNode, ExprNode](left, right, props).flatMap((transformer.transform(node) _).tupled)
        case node@EqualsNode(left, right, _) =>
          recBinary[ExprNode, ExprNode](left, right, props).flatMap((transformer.transform(node) _).tupled)
        case node@NotEqualsNode(left, right, _) =>
          recBinary[ExprNode, ExprNode](left, right, props).flatMap((transformer.transform(node) _).tupled)
        case node@LessThanNode(left, right, _) =>
          recBinary[ExprNode, ExprNode](left, right, props).flatMap((transformer.transform(node) _).tupled)
        case node@LessThanEqualsNode(left, right, _) =>
          recBinary[ExprNode, ExprNode](left, right, props).flatMap((transformer.transform(node) _).tupled)
        case node@GreaterThanNode(left, right, _) =>
          recBinary[ExprNode, ExprNode](left, right, props).flatMap((transformer.transform(node) _).tupled)
        case node@GreaterThanEqualsNode(left, right, _) =>
          recBinary[ExprNode, ExprNode](left, right, props).flatMap((transformer.transform(node) _).tupled)
        case node@RepetitionNode(condition, body, _) =>
          recBinary[ExprNode, StmtNode](condition, body, props).flatMap((transformer.transform(node) _).tupled)

        // Ternary nodes.
        case node@IfElseNode(condition, onTrue, onFalse, _) =>
          (rec[ExprNode](condition, props), rec[StmtNode](onTrue, props), rec[StmtNode](onFalse, props)).simultaneous
            .flatMap((transformer.transform(node) _).tupled)

        // Xary nodes.
        case node@TopLevelExprNode.ConstructorCallNode(_, arguments, _) => recXary(arguments, props).flatMap(transformer.transform(node))
        case node@TopLevelExprNode.ConstructNode(arguments, _, _) => recXary(arguments, props).flatMap(transformer.transform(node))
        case node@ConjunctionNode(expressions, _) => recXary(expressions, props).flatMap(transformer.transform(node))
        case node@DisjunctionNode(expressions, _) => recXary(expressions, props).flatMap(transformer.transform(node))
        case node@ConcatenationNode(expressions, _) => recXary(expressions, props).flatMap(transformer.transform(node))
        case node@TupleNode(expressions, _) => recXary(expressions, props).flatMap(transformer.transform(node))
        case node@ListNode(expressions, _) => recXary(expressions, props).flatMap(transformer.transform(node))
        case node@BlockNode(statements, _) => recXary(statements, props).flatMap(transformer.transform(node))
        case node@SimpleCallNode(_, _, arguments, _) => recXary(arguments, props).flatMap(transformer.transform(node))
        case node@FixedFunctionCallNode(_, _, arguments, _) => recXary(arguments, props).flatMap(transformer.transform(node))
        case node@DynamicCallNode(_, arguments, _) => recXary(arguments, props).flatMap(transformer.transform(node))

        // Map nodes.
        case node@MapNode(kvs, _) => kvs.map { kv =>
          (rec[ExprNode](kv.key, props), rec[ExprNode](kv.value, props)).simultaneous.flatMap((transformer.transform(kv) _).tupled)
        }.simultaneous.flatMap(transformer.transform(node))

        // Iteration nodes.
        case node@IterationNode(extractors, body, _) =>
          (
            extractors.map(extractor => rec[ExprNode](extractor.collection, props).flatMap(transformer.transform(extractor))).simultaneous,
            rec[StmtNode](body, props)
          ).simultaneous.flatMap((transformer.transform(node) _).tupled)
      }
    }
  }

  def transform(transformer: StmtTransformer)(node: StmtNode): Compilation[StmtNode] = new Applicator[Unit](transformer).transform(node, ())
}
