package lore.compiler.phases.verification

import lore.ast.visitor.StmtVisitor
import lore.ast.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.{Compilation, Registry}
import lore.types.{BasicType, ProductType, Type}

/**
  * Infers and checks expression types. After the type checker has been run without producing any compilation
  * errors, we can be sure that all expressions are soundly typed (barring compiler bugs, of course).
  */
object TypeChecker {

  class TypeCheckVisitor extends StmtVisitor[Type] {
    import StmtNode._
    import TopLevelExprNode._
    import ExprNode._

    implicit class StmtNodeExtension(node: StmtNode) {
      /**
        * Assigns the given type to the node and returns the type.
        */
      def typed(tpe: Type): Compilation[Type] = {
        node.setInferredType(tpe)
        Compilation.succeed(tpe)
      }
    }

    override def visitLeaf(node: StmtNode.LeafNode): Compilation[Type] = node match {
      // Variables.
      case VariableNode(name) =>
        // TODO: We need a sort of type context that saves local variable types etc. to resolve this.
        ???

      // Literals.
      case RealLiteralNode(_)   => node.typed(BasicType.Real)
      case IntLiteralNode(_)    => node.typed(BasicType.Int)
      case BoolLiteralNode(_)   => node.typed(BasicType.Boolean)
      case StringLiteralNode(_) => node.typed(BasicType.String)
      case UnitNode             => node.typed(ProductType.UnitType)
    }

    override def visitUnary(node: StmtNode.UnaryNode)(argument: Type): Compilation[Type] = node match {
      // Control nodes.
      case StmtNode.ReturnNode(expr) =>
        // TODO: Check that the returned expression adheres to the function's return type bounds.
        // This doesn't quite adhere to the spec, but we'll go with unit for now.
        // TODO: This should actually be a nothing/bottom type...
        node.typed(ProductType.UnitType)
      case TopLevelExprNode.YieldNode(expr) =>
        // TODO: Yield is special in that it determines the result type of the surrounding loop. We must create a
        //       "loop context" that we add all instances of yield to; then we try to find the lowest common type bound
        //       for all expressions once the context comes back to the list.
        node.typed(ProductType.UnitType)

      // Variable nodes.
      case TopLevelExprNode.VariableDeclarationNode(name, isMutable, tpe, value) =>
        // TODO: Add the variable type to the type context. Either infer the type from the value or, if a type has
        //       been explicitly declared, check that the value adheres to the type bounds.
        node.typed(ProductType.UnitType)
      case TopLevelExprNode.AssignmentNode(address, value) =>
        // TODO: Check that the value assigned to the variable adheres to the variable's type bounds.
        node.typed(ProductType.UnitType)

      // Unary operations.
      case ExprNode.NegationNode(expr) => ???
      case ExprNode.LogicalNotNode(expr) => ???
      case ExprNode.PropertyAccessNode(instance, names) => ???
    }

    override def visitBinary(node: BinaryNode)(left: Type, right: Type): Compilation[Type] = node match {
      // Repetitions.
      case RepeatWhileNode(condition, body, deferCheck) => ???

      // Binary operations.
      case AdditionNode(left, right) => ???
      case SubtractionNode(left, right) => ???
      case MultiplicationNode(left, right) => ???
      case DivisionNode(left, right) => ???
      case EqualsNode(left, right) => ???
      case NotEqualsNode(left, right) => ???
      case LessThanNode(left, right) => ???
      case LessThanEqualsNode(left, right) => ???
      case GreaterThanNode(left, right) => ???
      case GreaterThanEqualsNode(left, right) => ???
    }

    override def visitTernary(node: TernaryNode)(argument1: Type, argument2: Type, argument3: Type): Compilation[Type] = node match {
      case IfElseNode(condition, onTrue, onFalse) => ???
    }

    override def visitXary(node: XaryNode)(arguments: List[Type]): Compilation[Type] = node match {
      // Constructors.
      case ConstructorCallNode(name, arguments) => ???
      case ConstructNode(arguments, withSuper) => ???

      // Blocks.
      case BlockNode(statements) => ???

      // Literal constructions.
      case TupleNode(expressions) => ???
      case ListNode(expressions) => ???

      // Xary operations.
      case ConjunctionNode(expressions) => ???
      case DisjunctionNode(expressions) => ???
      case ConcatenationNode(expressions) => ???
      case CallNode(name, qualifier, arguments) => ???
      case FixedFunctionCallNode(name, types, arguments) => ???
    }

    override def visitMap(node: MapNode)(entries: List[(Type, Type)]): Compilation[Type] = {
      ???
    }

    override def visitIteration(node: IterationNode)(extractors: List[(String, Type)], body: Type): Compilation[Type] = {
      ???
    }

    override def before: PartialFunction[StmtNode, Unit] = {
      case ExprNode.RepeatWhileNode(_, _, _) | ExprNode.IterationNode(_, _) =>
      // TODO: Put a new yield context on the stack.
    }
  }

  def infer(expression: StmtNode)(implicit registry: Registry): Type = {
    ???
  }

}
